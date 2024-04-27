{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugin.TraceForeignCalls (plugin) where

import Prelude hiding ((<>))

import Control.Monad
import Data.Either (partitionEithers)

import GHC
import GHC.Plugins

import GHC.Builtin.Names
import GHC.Data.Bag
import GHC.Tc.Types
import GHC.Types.ForeignCall
import GHC.Types.SourceText

import Plugin.TraceForeignCalls.Instrument
import Plugin.TraceForeignCalls.Options
import Plugin.TraceForeignCalls.Util.GHC

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
    }

processRenamed ::
     [CommandLineOption]
  -> TcGblEnv
  -> HsGroup GhcRn
  -> TcM (TcGblEnv, HsGroup GhcRn)
processRenamed options tcGblEnv group = do
    runInstrument options $ (tcGblEnv,) <$> processGroup group

{-------------------------------------------------------------------------------
  Binding groups
-------------------------------------------------------------------------------}

processGroup :: HsGroup GhcRn -> Instrument (HsGroup GhcRn)
processGroup group@HsGroup{
                 hs_fords
               , hs_valds = XValBindsLR (NValBinds bindingGroups sigs)
               } = do
    (exports, imports) <- partitionEithers <$> mapM processForeignDecl hs_fords
    wrappers <- forM imports $ \i -> (i,) <$> mkWrapper i
    whenOption_ optionsDumpGenerated $ dumpWrappers wrappers
    let (newSigs, newValues) = unzip $ map snd wrappers
    return $ group {
        hs_fords = concat [
            exports
          , map reconstructForeignDecl imports
          ]
      , hs_valds =
          XValBindsLR $
            NValBinds
              (map trivialBindingGroup newValues ++ bindingGroups)
              (                        newSigs   ++ sigs         )
      }
processGroup HsGroup{hs_valds = ValBinds{}} =
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

reconstructForeignDecl :: ReplacedForeignImport -> LForeignDecl GhcRn
reconstructForeignDecl ReplacedForeignImport {
                           rfiSuffixedName
                         , rfiSigType
                         , rfiForeignImport
                         } =
    noLocA $ ForeignImport{
        fd_i_ext  = NoExtField
      , fd_name   = rfiSuffixedName
      , fd_sig_ty = rfiSigType
      , fd_fi     = rfiForeignImport
      }

processForeignDecl ::
     LForeignDecl GhcRn
  -> Instrument (Either (LForeignDecl GhcRn) ReplacedForeignImport)
processForeignDecl decl@(L _ ForeignExport{}) =
    return $ Left decl
processForeignDecl (L _ ForeignImport{
                       fd_i_ext  = NoExtField
                     , fd_name   = rfiOriginalName
                     , fd_sig_ty = rfiSigType
                     , fd_fi     = rfiForeignImport
                     }) = do
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

mkWrapper :: ReplacedForeignImport -> Instrument (LSig GhcRn, LHsBind GhcRn)
mkWrapper rfi@ReplacedForeignImport {
                  rfiOriginalName
                , rfiSuffixedName
                , rfiSigType = L _ sigType
                } = do
    (args, body)    <- mkWrapperBody rfi

    mHasCallStack :: Maybe (LHsType GhcRn) <-
      whenOption (not . optionsDisableCallStack) $ do
        hasCallStack <- findName nameHasCallStack
        return $ noLocA $ HsTyVar EpAnnNotUsed NotPromoted (noLocA hasCallStack)

    return (
        noLocA $
          TypeSig
            EpAnnNotUsed
            [rfiOriginalName]
            HsWC {
                hswc_ext  = []
              , hswc_body = noLocA $ sigType {
                    -- Signature as the original import but with HasCallStack
                    sig_body =
                      case mHasCallStack of
                        Nothing ->
                          sig_body sigType
                        Just hasCallStack -> noLocA $
                          HsQualTy {
                              hst_xqual = NoExtField
                            , hst_ctxt  = noLocA [hasCallStack]
                            , hst_body  = sig_body sigType
                            }
                  }
              }
      , noLocA $
          FunBind {
               fun_ext     = mkNameSet [unLoc rfiSuffixedName] -- TODO: what is this?
             , fun_id      = rfiOriginalName
             , fun_matches = MG {
#if __GLASGOW_HASKELL__ == 906
                   mg_ext  = Generated
#endif
#if __GLASGOW_HASKELL__ >= 908
                   mg_ext  = Generated SkipPmc
#endif
                 , mg_alts = noLocA . map noLocA $ [
                      Match {
                          m_ext   = EpAnnNotUsed
                        , m_ctxt  = FunRhs {
                              mc_fun        = rfiOriginalName
                            , mc_fixity     = Prefix
                            , mc_strictness = NoSrcStrict
                            }
                        , m_pats  = map namedVarPat args
                        , m_grhss = GRHSs {
                              grhssExt        = emptyComments
                            , grhssGRHSs      = map noLocA [
                                  GRHS
                                    EpAnnNotUsed
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
  -> Instrument ([Name], LHsExpr GhcRn)
mkWrapperBody rfi@ReplacedForeignImport {rfiSuffixedName, rfiSigType} = do
    traceEventIO <- findName nameTraceEventIO
    let callTraceEventIO :: LHsExpr GhcRn -> ExprLStmt GhcRn
        callTraceEventIO arg = noLocA $
            BodyStmt
              NoExtField
              (callNamedFn traceEventIO [arg])
              regularBodyStmt
              NoSyntaxExprRn

    evaluate <- findName nameEvaluate
    let callEvaluate :: LHsExpr GhcRn -> LHsExpr GhcRn
        callEvaluate arg = callNamedFn evaluate [arg]

    unsafePerformIO <- findName nameUnsafePerformIO
    let callUnsafePerformIO :: LHsExpr GhcRn -> LHsExpr GhcRn
        callUnsafePerformIO arg = callNamedFn unsafePerformIO [arg]

    (args, resultTy) <- uniqArgsFor (sig_body $ unLoc rfiSigType)
    let callUninstrumented :: LHsExpr GhcRn
        callUninstrumented = callLNamedFn rfiSuffixedName (map namedVar args)

    result         <- uniqInternalName "result"
    eventLogCall   <- mkEventLogCall rfi
    eventLogReturn <- mkEventLogReturn rfi
    let doBlock :: LHsExpr GhcRn
        doBlock = noLocA $ HsDo NoExtField (DoExpr Nothing) $ noLocA [
            callTraceEventIO eventLogCall
          , noLocA $
              BindStmt
                regularBindStmt
                (namedVarPat result)
                ( case checkIsIO resultTy of
                    Just _  -> callUninstrumented
                    Nothing -> callEvaluate callUninstrumented
                )
          , callTraceEventIO eventLogReturn
          , noLocA $
              LastStmt
                NoExtField
                (callNamedFn returnMName [namedVar result])
                Nothing
                NoSyntaxExprRn
          ]

    return (
        args
      , case checkIsIO resultTy of
          Just _  -> doBlock
          Nothing -> callUnsafePerformIO doBlock
      )

{-------------------------------------------------------------------------------
  Generate eventlog events
-------------------------------------------------------------------------------}

-- | Eventlog description for calling the foreign function
mkEventLogCall :: ReplacedForeignImport -> Instrument (LHsExpr GhcRn)
mkEventLogCall ReplacedForeignImport{
                        rfiOriginalName
                      , rfiForeignImport
                      } = do
    noCallStack <- asksOption optionsDisableCallStack

    if noCallStack then
      return $ stringExpr prefix
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
    return $ stringExpr $ concat [
        "trace-foreign-calls: return "
      , occNameString . nameOccName . unLoc $ rfiOriginalName
      ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

trivialBindingGroup :: LHsBind GhcRn -> (RecFlag, Bag (LHsBind GhcRn))
trivialBindingGroup binding = (NonRecursive, unitBag binding)

uniqInternalName :: String -> Instrument Name
uniqInternalName n = do
   resultUniq <- getUniqueM
   return $ mkInternalName resultUniq (mkVarOcc n) noSrcSpan

regularBodyStmt :: SyntaxExprRn
regularBodyStmt = SyntaxExprRn $ HsVar NoExtField (noLocA thenMName)

regularBindStmt :: XBindStmtRn
regularBindStmt =
    XBindStmtRn {
        xbsrn_bindOp = SyntaxExprRn $ HsVar NoExtField (noLocA bindMName)
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
emptyWhereClause = EmptyLocalBinds NoExtField

stringExpr :: String -> LHsExpr GhcRn
stringExpr = noLocA . HsLit EpAnnNotUsed . HsString NoSourceText . fsLit

callLNamedFn :: LIdP GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
callLNamedFn fn args = mkHsApps (noLocA $ HsVar NoExtField fn) args

callNamedFn :: Name -> [LHsExpr GhcRn] -> LHsExpr GhcRn
callNamedFn = callLNamedFn . noLocA

namedVar :: Name -> LHsExpr GhcRn
namedVar = noLocA . HsVar NoExtField . noLocA

namedVarPat :: Name -> LPat GhcRn
namedVarPat = noLocA . VarPat NoExtField . noLocA