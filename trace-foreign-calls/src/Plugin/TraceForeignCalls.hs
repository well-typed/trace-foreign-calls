{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plugin.TraceForeignCalls (plugin) where

import Prelude hiding ((<>))

import Control.Monad
import Data.Either (partitionEithers)

import GHC
import GHC.Plugins

import GHC.Builtin.Names qualified as Names
import GHC.Builtin.Types.Prim qualified as Prim
import GHC.LanguageExtensions qualified as LangExt
import GHC.Tc.Utils.Monad (TcM, TcGblEnv)
import GHC.Tc.Utils.Monad qualified as TC
import GHC.Types.ForeignCall qualified as Foreign
import GHC.Types.SourceFile (isHsBootOrSig)

import Plugin.TraceForeignCalls.GHC.Shim
import Plugin.TraceForeignCalls.GHC.Util
import Plugin.TraceForeignCalls.Instrument
import Plugin.TraceForeignCalls.Options

{-------------------------------------------------------------------------------
  Top-level

  References:

  - https://hackage.haskell.org/package/ghc-9.10.1
  - https://hackage.haskell.org/package/ghc-9.12.1

  - https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/extending_ghc.html#compiler-plugins
  - https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/exts/ffi.html
  - https://www.haskell.org/onlinereport/haskell2010/haskellch8.html
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      renamedResultAction = processRenamed
    , pluginRecompile     = purePlugin
    , driverPlugin        = \_ -> pure . enableUnliftedFFITypes
    }

enableUnliftedFFITypes :: HscEnv -> HscEnv
enableUnliftedFFITypes env = env {
      hsc_dflags = xopt_set (hsc_dflags env) LangExt.UnliftedFFITypes
    }

processRenamed ::
     [CommandLineOption]
  -> TcGblEnv
  -> HsGroup GhcRn
  -> TcM (TcGblEnv, HsGroup GhcRn)
processRenamed options tcGblEnv group
  | moduleUnit (TC.tcg_mod tcGblEnv) `elem` [primUnit, bignumUnit]
  = pure (tcGblEnv, group)

  | isHsBootOrSig (TC.tcg_src tcGblEnv)
  = pure (tcGblEnv, group)

  | otherwise
  = (tcGblEnv,) <$> runInstrument tcGblEnv options (processGroup group)

{-------------------------------------------------------------------------------
  Binding groups
-------------------------------------------------------------------------------}

processGroup :: HsGroup GhcRn -> Instrument (HsGroup GhcRn)
processGroup  group@HsGroup{
                 hs_fords
               , hs_valds = existingBindings
               } = do
    mTraceCCS <- findName nameTraceCCS

    -- Wrap all (possible) foreign imports
    (ignore, imports) <- partitionEithers <$> mapM processForeignDecl hs_fords
    wrappers <- forM imports $ \i -> (i,) <$> mkWrapper i

    -- Debug output
    dump <- optionsDumpGenerated <$> getOptions
    when dump $ dumpWrappers wrappers

    -- Construct modified module
    let (newSigs, newValues) = unzip $ map snd wrappers
    return $ group {
        hs_fords = concat [
            ignore
          , [importTraceCCS traceCCS | Just traceCCS <- [mTraceCCS]]
          , map reconstructForeignDecl imports
          ]
      , hs_valds =
          extendValBinds
            (map trivialBindingGroup newValues)
            newSigs
            existingBindings
      }

{-------------------------------------------------------------------------------
  Foreign declarations
-------------------------------------------------------------------------------}

data ReplacedForeignImport = ReplacedForeignImport {
      -- | The Haskell name the user intended for the foreign function (@foo@)
      rfiOriginalName :: LIdP GhcRn

      -- | The suffixed name (@foo_uninstrumented@)
    , rfiSuffixedName :: LIdP GhcRn

      -- | Type of the foreign function
    , rfiSigType :: LHsSigType GhcRn

      -- | The original (unmodified) foreign import
    , rfiForeignImport :: ForeignImport GhcRn

      -- | The type of this import
    , rfiImportType :: ForeignImportType
    }

data ForeignImportType =
    -- | Pure foreign import
    --
    -- We can only process these if we have access to @seq#@, so we record
    -- that evidence here.
    ForeignImportPure Name

    -- | Function in the IO monad
  | ForeignImportIO

-- | Import @traceCCS#@
--
-- Generates
--
-- > foreign import ccall unsafe "traceHeapProfSampleCostCentre"
-- >   traceCCS# :: Word8# -> Addr# -> Word64# -> IO ()
importTraceCCS :: Name -> LForeignDecl GhcRn
importTraceCCS traceCCS = noLocValue $
    ForeignImport {
        fd_i_ext  = noValue
      , fd_name   = noLocValue traceCCS
      , fd_sig_ty = noLocValue $
            HsSig noValue (HsOuterImplicit [])
          $ nlHsFunTy (nlHsTyVar NotPromoted Prim.word8PrimTyConName)
          $ nlHsFunTy (nlHsTyVar NotPromoted (tyConName Prim.addrPrimTyCon))
          $ nlHsFunTy (nlHsTyVar NotPromoted Prim.word64PrimTyConName)
          $ ioUnit
      , fd_fi =
          CImport
            noValue
            (noLocA Foreign.CCallConv)
            (noLocA Foreign.PlayRisky)
            Nothing -- header
            (CFunction $
               Foreign.StaticTarget
                 noValue
                 (fsLit "traceHeapProfSampleCostCentre")
                 Nothing -- unit
                 True    -- is this a function?
            )
      }

reconstructForeignDecl :: ReplacedForeignImport -> LForeignDecl GhcRn
reconstructForeignDecl ReplacedForeignImport {
                           rfiSuffixedName
                         , rfiSigType
                         , rfiForeignImport
                         } = noLocValue $
    ForeignImport{
        fd_i_ext  = noValue
      , fd_name   = rfiSuffixedName
      , fd_sig_ty = rfiSigType
      , fd_fi     = rfiForeignImport
      }

-- | Classify foreign declarations
--
-- Foreign declarations that we don't need to, or cannot handle, are returned
-- as Left values. These are:
--
-- * Foreign exports
-- * Foreign imports of primops (@prim@ calling convention)
processForeignDecl ::
     LForeignDecl GhcRn
  -> Instrument (Either (LForeignDecl GhcRn) ReplacedForeignImport)
processForeignDecl decl@(L _ ForeignExport{})
  = return $ Left decl
processForeignDecl decl@(L _ ForeignImport{
                       fd_i_ext  = NoExtField
                     , fd_name   = rfiOriginalName
                     , fd_sig_ty = rfiSigType
                     , fd_fi     = rfiForeignImport
                     })
  | CImport _ (unLoc -> conv) _ _ _ <- rfiForeignImport
  , conv == Foreign.PrimCallConv
  = return $ Left decl

  | checkIsIO rfiSigType
  = Right <$> aux ForeignImportIO

  | otherwise
  = do mSeq <- findName nameSeq
       case mSeq of
         Nothing      -> return $ Left decl
         Just seqHash -> Right <$> aux (ForeignImportPure seqHash)
  where
    aux :: ForeignImportType -> Instrument ReplacedForeignImport
    aux rfiImportType = do
        rfiSuffixedName <- renameForeignImport rfiOriginalName
        return ReplacedForeignImport{
            rfiOriginalName
          , rfiSuffixedName
          , rfiSigType
          , rfiForeignImport
          , rfiImportType
          }

dumpWrappers ::
     [(ReplacedForeignImport, (LSig GhcRn, LHsBind GhcRn))]
  -> Instrument ()
dumpWrappers =
    mapM_ (uncurry go)
  where
    go :: ReplacedForeignImport -> (LSig GhcRn, LHsBind GhcRn) -> Instrument ()
    go rfi (wrapperSig, wrapperVal) =
        printSimpleWarning (locA $ getLoc rfiOriginalName) $ vcat [
            "Replaced    " <> ppr rfiOriginalName
          , "with        " <> ppr rfiSuffixedName
          , "and wrapper " <> vcat [
                                  ppr wrapperSig
                                , ppr wrapperVal
                                ]
          ]
      where
        ReplacedForeignImport{
            rfiOriginalName
          , rfiSuffixedName
          } = rfi

{-------------------------------------------------------------------------------
  Plugin logic proper
-------------------------------------------------------------------------------}

renameForeignImport :: LIdP GhcRn -> Instrument (LIdP GhcRn)
renameForeignImport (L l n) = do
    uniq <- getUniqueM
    return $ L l $ mkDerivedInternalName aux uniq n
  where
    aux :: OccName -> OccName
    aux occ = mkOccNameFS (occNameSpace occ) $ mconcat [
          occNameFS occ
        , "_uninstrumented"
        ]

mkWrapper :: ReplacedForeignImport -> Instrument (LSig GhcRn, LHsBind GhcRn)
mkWrapper rfi@ReplacedForeignImport {
              rfiOriginalName
            , rfiSigType = L _ sigType
            } = do
    (args, body) <- mkWrapperBody rfi

    return (
        noLocValue $
          TypeSig
            noValue
            [rfiOriginalName]
            HsWC {
                hswc_ext  = []
              , hswc_body = noLocValue $ sigType {
                    sig_body = sig_body sigType
                  }
              }
      , mkSimpleFunBind rfiOriginalName [] args body
      )

-- | Make the body for the wrapper
--
-- Also returns the arguments to the wrapper
mkWrapperBody :: ReplacedForeignImport -> Instrument ([Name], LHsExpr GhcRn)
mkWrapperBody rfi = do
    mTraceCCS <- findName nameTraceCCS

    -- Construct call to the original function, for fresh args
    args <- uniqArgsFor (sig_body $ unLoc rfiSigType)
    let callOrig :: LHsExpr GhcRn
        callOrig = callLNamedFn rfiSuffixedName (map namedVar args)

    -- TODO:
    --
    -- * Add capability (and thread ID?) to event

    wrapped <-
      case rfiImportType of

        ForeignImportIO -> do
          unwrapIO "f" callOrig $ \f -> wrapIO $
            wrap mTraceCCS f $ \s ->
              -- Pass the RealWorld argument, actually running the function
              return $ mkHsApp f s

        ForeignImportPure seqHash -> do
          let_ "f" callOrig $ \f -> runIO $
            wrap mTraceCCS f $ \s -> return $
              -- For evaluation we use @seq#@, to guarantee ordering
              callNamedFn seqHash [f, s]

    return (args, wrapped)
  where
    ReplacedForeignImport{
        rfiSuffixedName
      , rfiSigType
      , rfiImportType
      } = rfi

    zero8Lit :: LHsExpr GhcRn
    zero8Lit = noLocValue $ HsLit noValue $ HsWord8Prim noValue 0

    eventLogCall, eventLogReturn :: LHsExpr GhcRn
    eventLogCall   = mkEventLogCall   rfi
    eventLogReturn = mkEventLogReturn rfi

    wrap ::
         Maybe Name
      -> LHsExpr GhcRn
      -> (RealWorld -> Instrument (LHsExpr GhcRn))
      -> (RealWorld -> Instrument (LHsExpr GhcRn))
    wrap Nothing         _ = withoutProfiling
    wrap (Just traceCCS) f = withProfiling traceCCS f

    withoutProfiling ::
         (RealWorld -> Instrument (LHsExpr GhcRn))
      -> (RealWorld -> Instrument (LHsExpr GhcRn))
    withoutProfiling call_f =
        callTraceEvent eventLogCall   $
        callIO ["result"] call_f      $ \[result] ->
        callTraceEvent eventLogReturn $
        returnIO result

    withProfiling ::
         Name
      -> LHsExpr GhcRn
      -> (RealWorld -> Instrument (LHsExpr GhcRn))
      -> (RealWorld -> Instrument (LHsExpr GhcRn))
    withProfiling traceCCS f call_f =
        callTraceEvent eventLogCall                        $
        callIO ["ccs"] (callNamedIO nameGetCurrentCCS [f]) $ \[ccs] ->
        callIO ["tid"] (callNamedIO nameMyThreadId [])     $ \[tid] ->
        getCapability tid                                  $ \cap   ->
        callTraceCCS traceCCS zero8Lit ccs cap             $
        callIO ["result"] call_f                           $ \[result] ->
        callTraceEvent eventLogReturn                      $
        returnIO result

{-------------------------------------------------------------------------------
  Generate eventlog events
-------------------------------------------------------------------------------}

-- | Eventlog description for calling the foreign function
mkEventLogCall :: ReplacedForeignImport -> LHsExpr GhcRn
mkEventLogCall ReplacedForeignImport{
                        rfiOriginalName
                      , rfiForeignImport
                      } =
    ubstringExpr prefix
  where
    prefix :: String
    prefix = concat [
          "trace-foreign-calls: call "
        , occNameString . nameOccName . unLoc $ rfiOriginalName
        , " ("
        , strCallConv
        , " "
        , strSafety
        , " "
        , show (strHeader ++ strCLabel)
        , ")"
        ]

    strCallConv, strSafety, strHeader, strCLabel :: String
    (strCallConv, strSafety, strHeader, strCLabel) =
        case rfiForeignImport of
          CImport _sourceText cCallConv safety mHeader cImportSpec -> (
              showSDocUnsafe $ ppr cCallConv
            , showSDocUnsafe $ ppr safety
            , case mHeader of
                Just (Foreign.Header _sourceText hdr) -> unpackFS hdr ++ " "
                Nothing                               -> ""
            , case cImportSpec of
                CLabel cLabel ->
                  unpackFS cLabel
                CFunction (Foreign.StaticTarget _sourceText cLabel _ _) ->
                  unpackFS cLabel
                CFunction Foreign.DynamicTarget ->
                  "<dynamic target>"
                CWrapper ->
                  "<wrapper>"
            )

-- | Eventlog description for the return of the foreign function
mkEventLogReturn :: ReplacedForeignImport -> LHsExpr GhcRn
mkEventLogReturn ReplacedForeignImport{rfiOriginalName} =
    ubstringExpr $ concat [
        "trace-foreign-calls: return "
      , occNameString . nameOccName . unLoc $ rfiOriginalName
      ]

{-------------------------------------------------------------------------------
  Auxiliary: constructions with fresh names
-------------------------------------------------------------------------------}

-- | Create unique name for each argument of the function
uniqArgsFor :: LHsType GhcRn -> Instrument [Name]
uniqArgsFor = go [] . unLoc
  where
    go :: [Name] -> HsType GhcRn -> Instrument [Name]
    go acc HsForAllTy{hst_body} = go acc (unLoc hst_body)
    go acc HsQualTy{hst_body}   = go acc (unLoc hst_body)
    go acc (HsFunTy _ _ _ rhs)  = do
        arg <- liftTcM $ uniqInternalName ("arg" ++ show (length acc))
        go (arg:acc) (unLoc rhs)
    go acc _otherTy =
        return $ reverse acc

-- | Bind to value without evaluating it
--
-- Given @e@, constructs
--
-- > let x = e in k x
--
-- for fresh @x@.
let_ :: m ~ Instrument
  => String
  -> LHsExpr GhcRn
  -> (LHsExpr GhcRn -> m (LHsExpr GhcRn))
  -> m (LHsExpr GhcRn)
let_ xNameHint e k = do
    x    <- liftTcM $ uniqInternalName xNameHint
    cont <- k (namedVar x)
    let binding :: LHsBind GhcRn
        binding = noLocA $ PatBind {
            pat_ext  = mkNameSet [x]
          , pat_lhs  = namedVarPat x
          , pat_mult = noValue
          , pat_rhs  = GRHSs {
                grhssExt        = noValue
              , grhssGRHSs      = [noLocA $ GRHS noValue [] e]
              , grhssLocalBinds = EmptyLocalBinds noValue
              }
          }
    return $ noLocValue $
        HsLet noValue (
            HsValBinds noValue $ mkValBinds [(NonRecursive, [binding])] []
          )
      $ cont

{-------------------------------------------------------------------------------
  Auxiliary: construct IO calls
-------------------------------------------------------------------------------}

type RealWorld = LHsExpr GhcRn

-- | Unwrap @IO@ action
--
-- Given @io@ and continuation @k@, constructs
--
-- > case io of IO f -> k f
--
-- for fresh @f@.
unwrapIO :: m ~ Instrument
  => String
  -> LHsExpr GhcRn
  -> (LHsExpr GhcRn -> m (LHsExpr GhcRn))
  -> m (LHsExpr GhcRn)
unwrapIO fNameHint io k = do
    f    <- liftTcM $ uniqInternalName fNameHint
    cont <- k (namedVar f)
    return $ noLocValue $
        HsCase CaseAlt io
      $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
      $ mkHsCaseAlt (noLocValue $
            ConPat
              noExtField
              (noLocValue Names.ioDataConName)
              (PrefixCon [] [namedVarPat f])
          )
      $ cont

-- | Wrap @IO@ action
--
-- Given @f@, constructs
--
-- > IO (\s -> f s)
--
-- for fresh @s@.
wrapIO :: m ~ Instrument
  => (RealWorld -> m (LHsExpr GhcRn))
  -> m (LHsExpr GhcRn)
wrapIO f = do
    s    <- liftTcM $ uniqInternalName "s"
    body <- f (namedVar s)
    return $
        mkHsApp (namedVar Names.ioDataConName)
      $ mkLambda [namedVarPat s]
      $ body

-- | Similar to 'wrapIO', but in a pure context (essentially @unsafePerformIO@)
--
-- Given @f@, constructs
--
-- > case runRW# (\s -> f (noDuplicate# s)) of (# _, result #) -> result
--
-- for fresh @s@ and @result@
runIO :: m ~ Instrument
  => (RealWorld -> m (LHsExpr GhcRn))
  -> m (LHsExpr GhcRn)
runIO f = do
    s      <- liftTcM $ uniqInternalName "s"
    result <- liftTcM $ uniqInternalName "result"
    runRW  <- findName nameRunRW
    noDup  <- findName nameNoDuplicate
    body   <- f $ callNamedFn noDup [namedVar s]
    let scrut = callNamedFn runRW [mkLambda [namedVarPat s] body]
    return $ noLocValue $
        HsCase CaseAlt scrut
      $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
      $ mkHsCaseAlt (noLocValue $
            TuplePat
              noExtField
              [ noLocValue (WildPat noExtField)
              , namedVarPat result
              ]
               Unboxed
          )
      $ namedVar result

-- | Return value in (low-level) IO monad
--
-- Constructs
--
-- > (# s, result #)
--
-- for given @s@ and @result@
returnIO :: m ~ Instrument
  => LHsExpr GhcRn
  -> RealWorld -> m (LHsExpr GhcRn)
returnIO result s =
    return $ noLocValue $
      ExplicitTuple
        noExtField
        [ Present noExtField s
        , Present noExtField result
        ]
        Unboxed

-- | Do low-level IO call
--
-- Given a context @IO (\s -> ..)@, @callIO f k s@ generates
--
-- > case f s of (# s', result #) -> k result s'
--
-- for fresh @s'@ and @result@
callIO :: m ~ Instrument
  => [String]
  -> (                   RealWorld -> m (LHsExpr GhcRn))
  -> ([LHsExpr GhcRn] -> RealWorld -> m (LHsExpr GhcRn))
  -> (                   RealWorld -> m (LHsExpr GhcRn))
callIO resultNameHints f k s = do
    s'      <- liftTcM $ uniqInternalName "s'"
    results <- liftTcM $ mapM uniqInternalName resultNameHints
    scrut   <- f s
    cont    <- k (map namedVar results) (namedVar s')
    return $ noLocValue $
        HsCase CaseAlt scrut
      $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
      $ mkHsCaseAlt (noLocValue $
            TuplePat
              noExtField
              (namedVarPat s' : map namedVarPat results)
              Unboxed
          )
      $ cont

-- | Like 'callIO', but for when the result is trivial @()@
callIO_ :: m ~ Instrument
  => (RealWorld -> m (LHsExpr GhcRn))
  -> (RealWorld -> m (LHsExpr GhcRn))
  -> (RealWorld -> m (LHsExpr GhcRn))
callIO_ f k = callIO ["_unit"] f (\[_unit] -> k)

-- | Call @traceEvent#@
--
-- This function is a little unusual, as it does not return a tuple.
--
-- > traceEvent# :: Addr# -> State# d -> State# d
callTraceEvent :: m ~ Instrument
  => LHsExpr GhcRn -- ^ String for the custom eent
  -> (RealWorld -> m (LHsExpr GhcRn))
  -> (RealWorld -> m (LHsExpr GhcRn))
callTraceEvent eventString k s = do
    s'    <- liftTcM $ uniqInternalName "s'"
    scrut <- callNamedIO nameTraceEvent [eventString] s
    cont  <- k (namedVar s')
    return $ noLocValue $
        HsCase CaseAlt scrut
      $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
      $ mkHsCaseAlt (namedVarPat s')
      $ cont

-- | Call @traceCCS#@
callTraceCCS :: m ~ Instrument
  => Name
  -> LHsExpr GhcRn -- ^ Profile ID (Word8#)
  -> LHsExpr GhcRn -- ^ Stack (Addr#)
  -> LHsExpr GhcRn -- ^ Residency (Word64#)
  -> (RealWorld -> m (LHsExpr GhcRn))
  -> (RealWorld -> m (LHsExpr GhcRn))
callTraceCCS traceCCS profileId ccs residency k s =
    unwrapIO "runTrace" call $ \runTrace ->
    callIO_ (return . mkHsApp runTrace) k s
  where
    call = mkHsApps (namedVar traceCCS) [profileId, ccs, residency]

-- | Get capability
--
-- Returns the capability as a 'Word64#'
getCapability :: m ~ Instrument
  => LHsExpr GhcRn -- ^ ThreadId#
  -> (LHsExpr GhcRn -> RealWorld -> m (LHsExpr GhcRn))
  -> (                 RealWorld -> m (LHsExpr GhcRn))
getCapability tid k s = do
    intToInt64    <- findName nameIntToInt64
    int64ToWord64 <- findName nameInt64ToWord64
    callIO
      ["_status", "cap", "_locked"]
      (callNamedIO nameThreadStatus [tid])
      (\[_status, cap, _locked] ->
         k $ mkLHsPar . mkHsApp (namedVar int64ToWord64)
           $ mkLHsPar . mkHsApp (namedVar intToInt64)
           $ cap
      )
      s

callNamedIO :: m ~ Instrument
  => (Names -> Name)
  -> [LHsExpr GhcRn]
  -> (RealWorld -> m (LHsExpr GhcRn))
callNamedIO f args s = do
    fName <- findName f
    return $ callNamedFn fName (args ++ [s])

