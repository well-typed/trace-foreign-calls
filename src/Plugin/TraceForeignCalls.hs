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

-- | Description of the foreign function to include in the eventlog
--
-- TODO: include more info
eventLogDescription :: ReplacedForeignImport -> String
eventLogDescription ReplacedForeignImport{rfiOriginalName} = concat [
      occNameString . nameOccName . unLoc $ rfiOriginalName
    ]

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
                , rfiSigType
                } = do
    (args, body) <- mkWrapperBody rfi
    return (
        noLocA $
          TypeSig
            EpAnnNotUsed
            [rfiOriginalName]
            HsWC {
                hswc_ext  = []
              , hswc_body = rfiSigType
              }
      , noLocA $
          FunBind {
               fun_ext     = mkNameSet [unLoc rfiSuffixedName] -- TODO: what is this?
             , fun_id      = rfiOriginalName
             , fun_matches = MG {
                   mg_ext  = Generated
                 , mg_alts = noLocA . map noLocA $ [
                      Match {
                          m_ext   = EpAnnNotUsed
                        , m_ctxt  = FunRhs {
                              mc_fun        = rfiOriginalName
                            , mc_fixity     = Prefix
                            , mc_strictness = NoSrcStrict
                            }
                        , m_pats  = map (noLocA . VarPat NoExtField) args
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
  -> Instrument ([LIdP GhcRn], LHsExpr GhcRn)
mkWrapperBody rfi@ReplacedForeignImport {rfiSuffixedName, rfiSigType} = do
    traceEventIO <- findName nameTraceEventIO
    let callTraceEventIO :: String -> ExprLStmt GhcRn
        callTraceEventIO label = noLocA $
            BodyStmt
              NoExtField
              ( noLocA $
                  HsApp
                    EpAnnNotUsed
                    (noLocA $ HsVar NoExtField (noLocA traceEventIO))
                    ( noLocA $ HsLit EpAnnNotUsed $ HsString NoSourceText $
                        fsLit $ label ++ eventLogDescription rfi
                    )
              )
              regularBodyStmt
              NoSyntaxExprRn

    evaluate <- findName nameEvaluate
    let callEvaluate :: LHsExpr GhcRn -> LHsExpr GhcRn
        callEvaluate arg = noLocA $
            HsApp
              EpAnnNotUsed
              (noLocA $ HsVar NoExtField (noLocA evaluate))
              arg

    unsafePerformIO <- findName nameUnsafePerformIO
    let callUnsafePerformIO :: LHsExpr GhcRn -> LHsExpr GhcRn
        callUnsafePerformIO arg = noLocA $
            HsApp
              EpAnnNotUsed
              (noLocA $ HsVar NoExtField (noLocA unsafePerformIO))
              arg

    (args, resultTy) <- uniqArgsFor (sig_body $ unLoc rfiSigType)
    let callUninstrumented :: LHsExpr GhcRn
        callUninstrumented =
            mkHsApps
              (noLocA $ HsVar NoExtField rfiSuffixedName)
              (map (noLocA . HsVar NoExtField) args)

    result <- uniqInternalName "result"
    let doBlock :: LHsExpr GhcRn
        doBlock = noLocA $ HsDo NoExtField (DoExpr Nothing) $ noLocA [
            callTraceEventIO "start foreign call "
          , noLocA $
              BindStmt
                regularBindStmt
                (noLocA $ VarPat NoExtField result)
                ( case checkIsIO resultTy of
                    Just _  -> callUninstrumented
                    Nothing -> callEvaluate callUninstrumented
                )
          , callTraceEventIO "stop foreign call "
          , noLocA $
              LastStmt
                NoExtField
                ( noLocA $
                    HsApp
                      EpAnnNotUsed
                      (noLocA $ HsVar NoExtField (noLocA returnMName))
                      (noLocA $ HsVar NoExtField result)
                )
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
  Auxiliary
-------------------------------------------------------------------------------}

trivialBindingGroup :: LHsBind GhcRn -> (RecFlag, Bag (LHsBind GhcRn))
trivialBindingGroup binding = (NonRecursive, unitBag binding)

uniqInternalName :: String -> Instrument (LIdP GhcRn)
uniqInternalName n = do
   resultUniq <- getUniqueM
   return $ noLocA $ mkInternalName resultUniq (mkVarOcc n) noSrcSpan

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
uniqArgsFor :: LHsType GhcRn -> Instrument ([LIdP GhcRn], LHsType GhcRn)
uniqArgsFor = go []
  where
    go ::
         [LIdP GhcRn]
      -> LHsType GhcRn
      -> Instrument ([LIdP GhcRn], LHsType GhcRn)
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
