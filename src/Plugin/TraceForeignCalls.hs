{-# LANGUAGE OverloadedStrings #-}

module Plugin.TraceForeignCalls (plugin) where

import Prelude hiding ((<>))
import Data.Maybe (mapMaybe)

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
               , hs_valds = ValBinds _annSortKey bindings sigs
               } = do
    replacements <- mapM processForeignDecl hs_fords
    dumpReplacements replacements
    let (newSigs, newValues) = replacementWrappers replacements
    return $ group {
        hs_fords = map replacementOrOriginal replacements
      , hs_valds =
          ValBinds
            NoAnnSortKey -- we don't care about precise pretty-printing
            (listToBag $ newValues ++ bagToList bindings)
            (            newSigs   ++ sigs              )
      }
processGroup group@HsGroup{
                 hs_fords
               , hs_valds = XValBindsLR (NValBinds bindingGroups sigs)
               } = do
    replacements <- mapM processForeignDecl hs_fords
    dumpReplacements replacements
    let (newSigs, newValues) = replacementWrappers replacements
    return $ group {
        hs_fords = map replacementOrOriginal replacements
      , hs_valds =
          XValBindsLR $
            NValBinds
              (map mkBindingGroup newValues ++ bindingGroups)
              (                   newSigs   ++ sigs         )
      }

{-------------------------------------------------------------------------------
  Foreign declarations
-------------------------------------------------------------------------------}

data ReplacedForeignDecl =
    ReplacedForeignDecl {
        replacedDecl    :: LForeignDecl GhcRn
      , replacedBy      :: LForeignDecl GhcRn
      , replacedWrapper :: (LSig GhcRn, LHsBind GhcRn)
      }
  | NotReplaced {
        originalDecl :: LForeignDecl GhcRn
      }

replacementOrOriginal :: ReplacedForeignDecl -> LForeignDecl GhcRn
replacementOrOriginal ReplacedForeignDecl{replacedBy} = replacedBy
replacementOrOriginal NotReplaced{originalDecl}       = originalDecl

replacementWrappers :: [ReplacedForeignDecl] -> ([LSig GhcRn], [LHsBind GhcRn])
replacementWrappers = unzip . mapMaybe aux
  where
    aux :: ReplacedForeignDecl -> Maybe (LSig GhcRn, LHsBind GhcRn)
    aux ReplacedForeignDecl{replacedWrapper = (sig, val)} = Just (sig, val)
    aux NotReplaced{}                                     = Nothing

processForeignDecl :: LForeignDecl GhcRn -> Instrument ReplacedForeignDecl
processForeignDecl decl@(L _ ForeignExport{}) =
    return $ NotReplaced decl
processForeignDecl decl@(L l ForeignImport{
                       fd_i_ext
                     , fd_name
                     , fd_sig_ty
                     , fd_fi
                     }) = do
    fd_name' <- renameForeignImport fd_name
    wrapper  <- mkWrapper (fd_name, fd_name') fd_sig_ty
    return ReplacedForeignDecl {
        replacedDecl    = decl
      , replacedBy      = L l ForeignImport{
                              fd_i_ext
                            , fd_name = fd_name'
                            , fd_sig_ty
                            , fd_fi
                            }
      , replacedWrapper = wrapper
      }

dumpReplacements :: [ReplacedForeignDecl] -> Instrument ()
dumpReplacements replacements =
    whenOption_ optionsDumpGenerated $
      mapM_ dumpReplacement replacements
  where
    dumpReplacement :: ReplacedForeignDecl -> Instrument ()
    dumpReplacement NotReplaced{} =
        return ()
    dumpReplacement ReplacedForeignDecl {
                        replacedDecl
                      , replacedBy
                      , replacedWrapper = (wrapperSig, wrapperVal)
                      } = do
        printSimpleWarning (locA $ getLoc replacedDecl) $ vcat [
            "Replaced    " <> ppr replacedDecl
          , "with        " <> ppr replacedBy
          , "and wrapper " <> vcat [
                                  ppr wrapperSig
                                , ppr wrapperVal
                                ]
          ]

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

mkWrapper ::
     (LIdP GhcRn, LIdP GhcRn)
  -> LHsSigType GhcRn
  -> Instrument (LSig GhcRn, LHsBind GhcRn)
mkWrapper (orig, renamed) sig = do
   args         <- uniqArgsFor (unLoc $ sig_body $ unLoc sig)
   result       <- uniqInternalName "result"
   traceEventIO <- findName nameTraceEventIO

   return (
       noLocA $
         TypeSig
           noAnn
           [wrapperName]
           HsWC {
               hswc_ext  = []
             , hswc_body = wrapperSig
             }
     , noLocA $
         FunBind {
              fun_ext     = mkNameSet [unLoc renamed] -- TODO: what is this?
            , fun_id      = wrapperName
            , fun_matches = MG {
                  mg_ext  = Generated OtherExpansion SkipPmc
                , mg_alts = noLocA . map noLocA $ [
                     Match {
                         m_ext   = noAnn
                       , m_ctxt  = FunRhs {
                             mc_fun        = wrapperName
                           , mc_fixity     = Prefix
                           , mc_strictness = NoSrcStrict
                           }
                       , m_pats  = map (noLocA . VarPat NoExtField) args
                       , m_grhss = GRHSs {
                             grhssExt        = emptyComments
                           , grhssGRHSs      = map noLocA [
                                 GRHS
                                   noAnn
                                   [] -- guards
                                   (noLocA $
                                     HsDo
                                       NoExtField
                                       (DoExpr Nothing)
                                       ( noLocA $
                                           wrapperBody
                                             traceEventIO
                                             args
                                             result
                                       )
                                   )
                               ]
                           , grhssLocalBinds = emptyWhereClause
                           }
                       }
                   ]
                }
           }
     )
  where
    wrapperName :: LIdP GhcRn
    wrapperName = orig

    wrapperNameString :: String
    wrapperNameString = occNameString . nameOccName . unLoc $ wrapperName

    wrapperSig :: LHsSigType GhcRn
    wrapperSig = sig

    wrapperBody ::
         Name
      -> [LIdP GhcRn]
      -> LIdP GhcRn
      -> [ExprLStmt GhcRn]
    wrapperBody traceEventIO args result = map noLocA [
          trace "start foreign call "
        , BindStmt
            regularBindStmt
            (noLocA $ VarPat NoExtField result)
            ( mkHsApps
                (noLocA $ HsVar NoExtField renamed)
                (map (noLocA . HsVar NoExtField) args)
            )
        , trace "stop foreign call "
        , LastStmt
            NoExtField
            ( noLocA $
                HsApp
                  noExtField
                  (noLocA $ HsVar NoExtField (noLocA returnMName))
                  (noLocA $ HsVar NoExtField result)
            )
            Nothing
            NoSyntaxExprRn
        ]
      where
        trace :: String -> ExprStmt GhcRn
        trace label =
            BodyStmt
              NoExtField
              ( noLocA $
                  HsApp
                    noExtField
                    (noLocA $ HsVar NoExtField (noLocA traceEventIO))
                    ( noLocA $ HsLit noExtField $ HsString NoSourceText $
                        fsLit $ label ++ wrapperNameString
                    )
              )
              regularBodyStmt
              NoSyntaxExprRn

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkBindingGroup :: LHsBind GhcRn -> (RecFlag, Bag (LHsBind GhcRn))
mkBindingGroup binding = (NonRecursive, unitBag binding)

emptyWhereClause :: HsLocalBinds GhcRn
emptyWhereClause = EmptyLocalBinds NoExtField

regularBindStmt :: XBindStmtRn
regularBindStmt =
    XBindStmtRn {
        xbsrn_bindOp = SyntaxExprRn $ HsVar NoExtField (noLocA bindMName)
      , xbsrn_failOp = Nothing
      }

regularBodyStmt :: SyntaxExprRn
regularBodyStmt = SyntaxExprRn $ HsVar NoExtField (noLocA thenMName)

uniqArgsFor :: HsType GhcRn -> Instrument [LIdP GhcRn]
uniqArgsFor = go []
  where
    go :: [LIdP GhcRn] -> HsType GhcRn -> Instrument [LIdP GhcRn]
    go acc HsForAllTy{hst_body} =
        go acc $ unLoc hst_body
    go acc HsQualTy{hst_body} =
        go acc $ unLoc hst_body
    go acc (HsFunTy _ _ _lhs rhs) = do
        arg <- uniqInternalName ("arg" ++ show (length acc))
        go (arg:acc) $ unLoc rhs
    go acc _otherwise =
        return $ reverse acc

uniqInternalName :: String -> Instrument (LIdP GhcRn)
uniqInternalName n = do
   resultUniq <- getUniqueM
   return $ noLocA $ mkInternalName resultUniq (mkVarOcc n) noSrcSpan


