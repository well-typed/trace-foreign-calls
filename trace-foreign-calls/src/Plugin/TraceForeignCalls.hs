{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

module Plugin.TraceForeignCalls (plugin) where

import Prelude hiding ((<>))

import Control.Monad
import Data.Either (partitionEithers)
import Data.Maybe

import GHC
import GHC.Plugins

import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Data.Bag
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Types.ForeignCall
import GHC.Types.SourceFile
import GHC.Types.SourceText
import GHC.Platform.Ways
import qualified GHC.LanguageExtensions as LangExt
import GHC.Unit.Env

import Plugin.TraceForeignCalls.Instrument
import Plugin.TraceForeignCalls.Options
import Plugin.TraceForeignCalls.Util.GHC
import Plugin.TraceForeignCalls.Util.Shim

{-------------------------------------------------------------------------------
  Top-level

  References:

  - https://downloads.haskell.org/ghc/9.6.4/docs/users_guide/extending_ghc.html#compiler-plugins
  - https://hackage.haskell.org/package/ghc-9.6.4
  - https://downloads.haskell.org/ghc/9.6.4/docs/users_guide/exts/ffi.html
  - https://www.haskell.org/onlinereport/haskell2010/haskellch8.html
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      renamedResultAction = processRenamed
    , pluginRecompile     = purePlugin
    , driverPlugin = \_ env -> pure $ env { hsc_dflags = xopt_set (hsc_dflags env) LangExt.UnliftedFFITypes }
    }

processRenamed ::
     [CommandLineOption]
  -> TcGblEnv
  -> HsGroup GhcRn
  -> TcM (TcGblEnv, HsGroup GhcRn)
processRenamed options tcGblEnv group
    | moduleUnit (tcg_mod tcGblEnv) `elem` [primUnit, bignumUnit] = pure (tcGblEnv, group)
    | isHsBootOrSig (tcg_src tcGblEnv) = pure (tcGblEnv, group)
    | otherwise = do
        df <- getDynFlags
        hsc <- getTopEnv
        let haveSeq = if moduleUnit (tcg_mod tcGblEnv) == ghcInternalUnit
                      then isJust $ lookupHugByModule gHC_INTERNAL_IO (hsc_HUG hsc)
                      else True
        runInstrument (sccProfilingEnabled df) options $ (tcGblEnv,) <$> processGroup (ways df `hasWay` WayProf) haveSeq group

{-------------------------------------------------------------------------------
  Binding groups
-------------------------------------------------------------------------------}

processGroup :: Bool -> Bool -> HsGroup GhcRn -> Instrument (HsGroup GhcRn)
processGroup profiling haveSeq group@HsGroup{
                 hs_fords
               , hs_valds = XValBindsLR (NValBinds bindingGroups sigs)
               } = do
    traceCCSFI <- whenOption (\opts -> profiling && not (optionsDisableCallStack opts)) mkTraceCCS
    (exports, imports) <- partitionEithers <$> mapM processForeignDecl hs_fords
    wrappers <- forM imports $ \i -> (i,) <$> mkWrapper (unLoc . fd_name . unLoc <$> traceCCSFI) haveSeq i
    whenOption_ optionsDumpGenerated $ dumpWrappers wrappers
    let (newSigs, newValues) = unzip $ map snd wrappers
    return $ group {
        hs_fords = concat [
            exports
          , maybeToList traceCCSFI
          , map reconstructForeignDecl imports
          ]
      , hs_valds =
          XValBindsLR $
            NValBinds
              (map trivialBindingGroup newValues ++ bindingGroups)
              (                        newSigs   ++ sigs         )
      }
processGroup _ _ HsGroup{hs_valds = ValBinds{}} =
    error "impossible (ValBinds is only used before renaming)"

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
    }

mkTraceCCS :: Instrument (LForeignDecl GhcRn)
mkTraceCCS = do
  name <- uniqInternalName "traceCCS#"
  let iou = nlHsAppTy (nlHsTyVar NotPromoted ioTyConName) (nlHsTyVar NotPromoted (tyConName unitTyCon))
  return $ noLocValue $ ForeignImport
    { fd_i_ext = noValue
    , fd_name = noLocValue name
    , fd_sig_ty = noLocValue
                $ HsSig noValue (HsOuterImplicit [])
                $ nlHsFunTy (nlHsTyVar NotPromoted word8PrimTyConName)
                $ nlHsFunTy (nlHsTyVar NotPromoted (tyConName addrPrimTyCon))
                $ nlHsFunTy (nlHsTyVar NotPromoted word64PrimTyConName) iou
    , fd_fi = CImport (L noSrcSpanA NoSourceText) (L noSrcSpanA CCallConv) (L noSrcSpanA PlayRisky) Nothing (CFunction $ StaticTarget NoSourceText (fsLit "traceHeapProfSampleCostCentre") Nothing True)
    }
reconstructForeignDecl :: ReplacedForeignImport -> LForeignDecl GhcRn
reconstructForeignDecl ReplacedForeignImport {
                           rfiSuffixedName
                         , rfiSigType
                         , rfiForeignImport
                         } =
    noLocValue $ ForeignImport{
        fd_i_ext  = noValue
      , fd_name   = rfiSuffixedName
      , fd_sig_ty = rfiSigType
      , fd_fi     = rfiForeignImport
      }

processForeignDecl ::
     LForeignDecl GhcRn
  -> Instrument (Either (LForeignDecl GhcRn) ReplacedForeignImport)
processForeignDecl decl@(L _ ForeignExport{}) =
    return $ Left decl
processForeignDecl decl@(L _ ForeignImport{
                       fd_i_ext  = NoExtField
                     , fd_name   = rfiOriginalName
                     , fd_sig_ty = rfiSigType
                     , fd_fi     = rfiForeignImport
                     })
    | CImport _ (unLoc -> conv) _ _ _ <- rfiForeignImport
    , conv == PrimCallConv = return $ Left decl
    | otherwise = do
    rfiSuffixedName <- renameForeignImport rfiOriginalName
    return $ Right ReplacedForeignImport{
        rfiOriginalName
      , rfiSuffixedName
      , rfiSigType
      , rfiForeignImport
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

mkWrapper :: Maybe Name -> Bool -> ReplacedForeignImport -> Instrument (LSig GhcRn, LHsBind GhcRn)
mkWrapper traceCCS haveSeq rfi@ReplacedForeignImport {
                  rfiOriginalName
                , rfiSuffixedName
                , rfiSigType = L _ sigType
                } = do
    (args, body)    <- mkWrapperBody rfi traceCCS haveSeq

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
      , noLocValue $
          FunBind {
               fun_ext     = mkNameSet [unLoc rfiSuffixedName] -- TODO: what is this?
             , fun_id      = rfiOriginalName
             , fun_matches = MG {
                   mg_ext  = originGenerated
                 , mg_alts = noLocValue . map noLocValue $ [
                      Match {
                          m_ext   = noValue
                        , m_ctxt  = FunRhs {
                              mc_fun        = rfiOriginalName
                            , mc_fixity     = Prefix
                            , mc_strictness = NoSrcStrict
                            , mc_an = AnnFunRhs NoEpTok [] []
                            }
                        , m_pats  =
#if MIN_VERSION_ghc(9,12,0)
                            noLocValue $
#endif
                            map namedVarPat args
                        , m_grhss = GRHSs {
                              grhssExt        = emptyComments
                            , grhssGRHSs      = map noLocValue [
                                  GRHS
                                    noValue
                                    [] -- guards
                                    body
                                ]
                            , grhssLocalBinds = emptyWhereClause
                            }
                        }
                    ]
                 }
            }
      )

-- | Make the body for the wrapper
--
-- Also returns the arguments to the wrapper
mkWrapperBody ::
     ReplacedForeignImport
  -> Maybe Name -> Bool -> Instrument ([Name], LHsExpr GhcRn)
mkWrapperBody rfi@ReplacedForeignImport {rfiSuffixedName, rfiSigType} mtraceCCS haveSeq = do
    traceEventHash <- findName nameTraceEventHash
    let callTraceEvent :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
        callTraceEvent arg st = callNamedFn traceEventHash [arg, st]

    mseqHash <- if haveSeq then Just <$> findName nameSeq else pure Nothing

    runRW <- findName nameRunRW
    let callRunRW :: LHsExpr GhcRn -> LHsExpr GhcRn
        callRunRW arg = callNamedFn runRW [arg]

    noDuplicate <- findName nameNoDuplicate
    let callNoDuplicate :: LHsExpr GhcRn -> LHsExpr GhcRn
        callNoDuplicate arg = callNamedFn noDuplicate [arg]

    (args, resultTy) <- uniqArgsFor (sig_body $ unLoc rfiSigType)
    let callUninstrumented :: LHsExpr GhcRn
        callUninstrumented = callLNamedFn rfiSuffixedName (map namedVar args)

    getCurrentCCS <- findName nameGetCurrentCCS
    let callGetCurrentCCS :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
        callGetCurrentCCS arg st = callNamedFn getCurrentCCS [arg, st]

    let zero8Lit = noLocValue $ HsLit noValue $ HsWord8Prim NoSourceText 0
        zero64Lit = noLocValue $ HsLit noValue $ HsWord64Prim NoSourceText 0

    s              <- uniqInternalName "s"
    s'             <- uniqInternalName "s'"
    s''            <- uniqInternalName "s''"
    s'''           <- uniqInternalName "s'''"
    f              <- uniqInternalName "f"
    ccs            <- uniqInternalName "ccs"
    runTrace       <- uniqInternalName "runTrace"
    result         <- uniqInternalName "result"
    result'        <- uniqInternalName "result'"
    eventLogCall   <- mkEventLogCall rfi
    eventLogReturn <- mkEventLogReturn rfi

    let wrapped = case checkIsIO resultTy of
            {- case foo of
                 IO f -> IO (\s -> case f (traceEvent# call s) of
                                     (# s', result #) -> (# traceEvent# return s', result #))
                 ---------------------------------------------------------------------------- (traceCCS)
                 IO f -> IO (\s -> case getCurrentCCS# f (traceEvent# call s) of
                                     (# s', ccs #) -> case traceCCS 0 ccs 0 of
                                       IO runTrace -> case runTrace s' of
                                         (# s'', _ ) -> case f s'' of
                                           (# s''', result #) -> (# traceEvent# return s''', result #))
            -}
            Just _ -> HsCase CaseAlt callUninstrumented
                    $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                    $ mkHsCaseAlt (noLocValue $ ConPat noExtField (noLocValue ioDataConName) $ PrefixCon [] [namedVarPat f])
                    $ mkHsApp (namedVar ioDataConName)
                    $ mkHsLam (noLocValue [namedVarPat s])
                    $ noLocValue
                    $ case mtraceCCS of
                        Nothing ->
                            HsCase CaseAlt (mkHsApp (namedVar f) (callTraceEvent eventLogCall $ namedVar s))
                          $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                          $ mkHsCaseAlt (noLocValue $ TuplePat noExtField [namedVarPat s', namedVarPat result] Unboxed)
                          $ noLocValue $ ExplicitTuple noExtField [Present noExtField (callTraceEvent eventLogReturn (namedVar s')), Present noExtField (namedVar result) ] Unboxed
                        Just traceCCS ->
                            HsCase CaseAlt (callGetCurrentCCS (namedVar f) (callTraceEvent eventLogCall $ namedVar s))
                          $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                          $ mkHsCaseAlt (noLocValue $ TuplePat noExtField [namedVarPat s', namedVarPat ccs] Unboxed)
                          $ noLocValue $ HsCase CaseAlt (mkHsApp (mkHsApp (mkHsApp (namedVar traceCCS) zero8Lit) (namedVar ccs)) zero64Lit)
                          $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                          $ mkHsCaseAlt (noLocValue $ ConPat noExtField (noLocValue ioDataConName) $ PrefixCon [] [namedVarPat runTrace])
                          $ noLocValue $ HsCase CaseAlt (mkHsApp (namedVar runTrace) (namedVar s'))
                          $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                          $ mkHsCaseAlt (noLocValue $ TuplePat noExtField [namedVarPat s'', noLocValue (WildPat noExtField)] Unboxed)
                          $ noLocValue $ HsCase CaseAlt (mkHsApp (namedVar f) (namedVar s''))
                          $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                          $ mkHsCaseAlt (noLocValue $ TuplePat noExtField [namedVarPat s''', namedVarPat result] Unboxed)
                          $ noLocValue $ ExplicitTuple noExtField [Present noExtField (callTraceEvent eventLogReturn (namedVar s''')), Present noExtField (namedVar result) ] Unboxed

      
            {- case runRW# (\s -> case seq# foo (traceEvent# call (noDuplicate s)) of
                     (# s', result #) -> (# traceEvent# return s', result #)) of
                 (# _ , result' #) -> result'
            -}
            Nothing
              | Just seqHash <- mseqHash
              , let callSeq arg st = callNamedFn seqHash [arg, st]
              -> HsCase CaseAlt
                 ( callRunRW
                 $ mkHsLam (noLocValue [namedVarPat s])
                 $ noLocValue
                 $ HsCase CaseAlt
                        (callSeq callUninstrumented $ callTraceEvent eventLogCall $ callNoDuplicate $ namedVar s)
                 $ mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                 $ mkHsCaseAlt (noLocValue $ TuplePat noExtField [namedVarPat s', namedVarPat result] Unboxed)
                 $ noLocValue $ ExplicitTuple noExtField [Present noExtField (callTraceEvent eventLogReturn (namedVar s')), Present noExtField (namedVar result) ] Unboxed
                 )
                 ( mkMatchGroup (Generated OtherExpansion SkipPmc) . noLocValue . pure
                 $ mkHsCaseAlt (noLocValue $ TuplePat noExtField [noLocValue (WildPat noExtField), namedVarPat result'] Unboxed)
                               (namedVar result'))
             | otherwise -> unLoc callUninstrumented

    return ( args , noLocValue wrapped )

{-------------------------------------------------------------------------------
  Generate eventlog events
-------------------------------------------------------------------------------}

-- | Eventlog description for calling the foreign function
mkEventLogCall :: ReplacedForeignImport -> Instrument (LHsExpr GhcRn)
mkEventLogCall ReplacedForeignImport{
                        rfiOriginalName
                      , rfiForeignImport
                      } = do
    noCallStack <- pure True -- asksOption optionsDisableCallStack

    if noCallStack then
      return $ ubstringExpr prefix
    else do
      callStack        <- findName nameCallStack
      prettyCalllStack <- findName namePrettyCallStack
      return $ callNamedFn appendName [
          stringExpr (prefix ++ " at ")
        , callNamedFn prettyCalllStack [namedVar callStack]
        ]
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
                Just (Header _sourceText hdr) -> unpackFS hdr ++ " "
                Nothing                       -> ""
            , case cImportSpec of
                CLabel cLabel ->
                  unpackFS cLabel
                CFunction (StaticTarget _sourceText cLabel _ _) ->
                  unpackFS cLabel
                CFunction DynamicTarget ->
                  "<dynamic target>"
                CWrapper ->
                  "<wrapper>"
            )

-- | Eventlog description for the return of the foreign function
mkEventLogReturn :: ReplacedForeignImport -> Instrument (LHsExpr GhcRn)
mkEventLogReturn ReplacedForeignImport{rfiOriginalName} = do
    return $ ubstringExpr $ concat [
        "trace-foreign-calls: return "
      , occNameString . nameOccName . unLoc $ rfiOriginalName
      ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

#if MIN_VERSION_ghc(9,12,0)
trivialBindingGroup :: LHsBind GhcRn -> (RecFlag,  [LHsBind GhcRn])
trivialBindingGroup binding = (NonRecursive, [binding])
#else
trivialBindingGroup :: LHsBind GhcRn -> (RecFlag, Bag (LHsBind GhcRn))
trivialBindingGroup binding = (NonRecursive, unitBag binding)
#endif

uniqInternalName :: String -> Instrument Name
uniqInternalName n = do
   resultUniq <- getUniqueM
   return $ mkInternalName resultUniq (mkVarOcc n) noSrcSpan

regularBodyStmt :: SyntaxExprRn
regularBodyStmt = SyntaxExprRn $ HsVar noValue (noLocValue thenMName)

regularBindStmt :: XBindStmtRn
regularBindStmt =
    XBindStmtRn {
        xbsrn_bindOp = SyntaxExprRn $ HsVar noValue (noLocValue bindMName)
      , xbsrn_failOp = Nothing
      }

-- | Create unique name for each argument of the function
--
-- Also returns the result type.
uniqArgsFor :: LHsType GhcRn -> Instrument ([Name], LHsType GhcRn)
uniqArgsFor = go []
  where
    go ::
         [Name]
      -> LHsType GhcRn
      -> Instrument ([Name], LHsType GhcRn)
    go acc (L _ HsForAllTy{hst_body}) =
        go acc hst_body
    go acc (L _ HsQualTy{hst_body}) =
        go acc hst_body
    go acc (L _ (HsFunTy _ _ _lhs rhs)) = do
        arg <- uniqInternalName ("arg" ++ show (length acc))
        go (arg:acc) rhs
    go acc otherTy =
        return (reverse acc, otherTy)

-- | Match against @IO a@ for some @a@
checkIsIO :: LHsType GhcRn -> Maybe (LHsType GhcRn)
checkIsIO (L _ ty) =
    case ty of
      HsAppTy _ (L _ (HsTyVar _ _ (L _ io))) b | io == ioTyConName ->
        Just b
      _otherwise ->
        Nothing

emptyWhereClause :: HsLocalBinds GhcRn
emptyWhereClause = EmptyLocalBinds noValue

stringExpr :: String -> LHsExpr GhcRn
stringExpr = noLocValue . HsLit noValue . HsString NoSourceText . fsLit


ubstringExpr :: String -> LHsExpr GhcRn
ubstringExpr = noLocValue . HsLit noValue . mkHsStringPrimLit . fsLit

callLNamedFn :: LIdP GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
callLNamedFn fn args = mkHsApps (noLocValue $ HsVar noValue fn) args

callNamedFn :: Name -> [LHsExpr GhcRn] -> LHsExpr GhcRn
callNamedFn = callLNamedFn . noLocValue

namedVar :: Name -> LHsExpr GhcRn
namedVar = noLocValue . HsVar noValue . noLocValue

namedVarPat :: Name -> LPat GhcRn
namedVarPat = noLocValue . VarPat noValue . noLocValue
