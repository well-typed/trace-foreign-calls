{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Plugin.TraceForeignCalls.GHC.Util (
    -- * Access to 'HscEnv'
    HasHscEnv(..)
    -- * Errors and warnings
  , throwSimpleError
  , printSimpleWarning
    -- * Names
  , resolveVarName
  , resolveTcName
  , uniqInternalName
    -- * Annotations
  , NoValue(..)
  , noLocValue
    -- * Helpers for constructing bits of the AST
  , trivialBindingGroup
  , checkIsIO
  , emptyWhereClause
  , ubstringExpr
  , callLNamedFn
  , callNamedFn
  , namedLVar
  , namedVar
  , namedVarPat
  , ioUnit
  ) where

import GHC hiding (getNamePprCtx)
import GHC.Plugins hiding (getNamePprCtx, getHscEnv)

import GHC.Builtin.Names qualified as Names
import GHC.Data.IOEnv
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Rename.Env
import GHC.Runtime.Context
import GHC.Tc.Types
import GHC.Types.Error
import GHC.Types.SourceText
import GHC.Utils.Error
import GHC.Utils.Logger

{-------------------------------------------------------------------------------
  Access to 'HscEnv'
-------------------------------------------------------------------------------}

class (MonadIO m, HasDynFlags m) => HasHscEnv m where
  getHscEnv :: m HscEnv

instance HasHscEnv TcM where
  getHscEnv = env_top <$> getEnv

{-------------------------------------------------------------------------------
  Internal auxiliary: using the 'HscEnv'
-------------------------------------------------------------------------------}

getNamePprCtx :: HasHscEnv m => m NamePprCtx
getNamePprCtx =
    aux <$> getHscEnv
  where
    aux :: HscEnv -> NamePprCtx
    aux HscEnv{hsc_unit_env, hsc_IC} = icNamePprCtx hsc_unit_env hsc_IC

getDiagOpts :: HasHscEnv m => m DiagOpts
getDiagOpts = initDiagOpts <$> getDynFlags

{-------------------------------------------------------------------------------
  Errors and warnings
-------------------------------------------------------------------------------}

throwSimpleError :: HasHscEnv m => SrcSpan -> SDoc -> m a
throwSimpleError l doc = do
    namePprCtx <- getNamePprCtx
    throwOneError $ mkErrorMsgEnvelope l namePprCtx (ghcUnknownMessage diag)
  where
    diag :: DiagnosticMessage
    diag = DiagnosticMessage {
      diagMessage = mkSimpleDecorated doc
    , diagReason  = ErrorWithoutFlag
    , diagHints   = []
    }

printSimpleWarning :: (HasHscEnv m, HasLogger m) => SrcSpan -> SDoc -> m ()
printSimpleWarning l doc = do
    logger     <- getLogger
    diagOpts   <- getDiagOpts
    namePprCtx <- getNamePprCtx
    liftIO $
      printMessages @DiagnosticMessage
        logger
        NoDiagnosticOpts
        diagOpts
        (singleMessage $ mkMsgEnvelope diagOpts l namePprCtx diag)
  where
    diag :: DiagnosticMessage
    diag = DiagnosticMessage {
          diagMessage = mkSimpleDecorated $ pprSetDepth AllTheWay doc
        , diagReason  = WarningWithoutFlag
        , diagHints   = []
        }

{-------------------------------------------------------------------------------
  Names

  If we use 'Qual' for the 'RdrName' then the module needs to have that module
  imported. We could /add/ the import, but that has problems of its own
  (spurious warnings). We therefore use 'Orig'; this does mean we need to
  provide a unit, but we only lok things up from base (we'd have to change this
  once we have the ghc-internals split). It also means we have to import the
  definition from the /defining/ module, rather than it's true "home base" (it's
  canonical exporting module).

  A much simpler approach is to depend on TH to resolve names, and use
  'thNameToGhcNameIO'. However, at present the resulting dependency on
  @template-haskell@ would make the plugin unuseable for base or the boot
  modules.
-------------------------------------------------------------------------------}

resolveVarName :: Module -> String -> TcM Name
resolveVarName = resolveName mkVarOcc

resolveTcName :: Module -> String -> TcM Name
resolveTcName = resolveName mkTcOcc

-- | Internal generalization
resolveName :: (String -> OccName) -> Module -> String -> TcM Name
resolveName f modl name = lookupOccRn $ Orig modl (f name)

uniqInternalName :: String -> TcM Name
uniqInternalName n = do
   resultUniq <- getUniqueM
   return $ mkInternalName resultUniq (mkVarOcc n) noSrcSpan

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

class NoValue a where
  -- | Value that provides no additional information
  noValue :: a

noLocValue :: NoValue l => e -> GenLocated l e
noLocValue = L noValue

instance (NoValue l, NoValue e) => NoValue (GenLocated l e) where
  noValue = noLocValue noValue

instance NoValue NoExtField where
  noValue = NoExtField

instance NoValue SrcSpan where
  noValue = noSrcSpan

instance NoValue SourceText where
  noValue = NoSourceText

instance NoValue EpAnnComments where
  noValue = emptyComments

instance NoValue (AnnSortKey tag) where
  noValue = NoAnnSortKey

instance (XNoMultAnn pass ~ ann, NoValue ann) => NoValue (HsMultAnn pass) where
  noValue = HsNoMultAnn noValue

instance NoAnn ann => NoValue (EpAnn ann) where
  noValue = noAnn

instance NoValue AnnSig where
  noValue = noAnn

instance NoValue EpaLocation where
  noValue = noAnn

{-------------------------------------------------------------------------------
  Helpers for constructing bits of the AST
-------------------------------------------------------------------------------}

trivialBindingGroup :: LHsBind GhcRn -> (RecFlag,  [LHsBind GhcRn])
trivialBindingGroup binding = (NonRecursive, [binding])

-- | Check if a function signature returns something in the @IO@ monad
checkIsIO :: LHsSigType GhcRn -> Bool
checkIsIO = go . unLoc . sig_body . unLoc
  where
    go :: HsType GhcRn -> Bool
    go HsForAllTy{hst_body} = go (unLoc hst_body)
    go HsQualTy{hst_body}   = go (unLoc hst_body)
    go (HsFunTy _ _ _ rhs)  = go (unLoc rhs)
    go ty =
        case ty of
          HsAppTy _ (L _ (HsTyVar _ _ (L _ io))) _ | io == Names.ioTyConName ->
            True
          _otherwise ->
            False

emptyWhereClause :: HsLocalBinds GhcRn
emptyWhereClause = EmptyLocalBinds noValue

ubstringExpr :: String -> LHsExpr GhcRn
ubstringExpr = noLocValue . HsLit noValue . mkHsStringPrimLit . fsLit

callLNamedFn :: LIdP GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
callLNamedFn fn args =
    mkHsApps (noLocValue $ HsVar noValue fn) $
      map mkLHsPar args

callNamedFn :: IdP GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
callNamedFn = callLNamedFn . noLocValue

namedLVar :: LIdP GhcRn -> LHsExpr GhcRn
namedLVar = noLocValue . HsVar noValue

namedVar :: IdP GhcRn -> LHsExpr GhcRn
namedVar = namedLVar . noLocValue

namedVarPat :: Name -> LPat GhcRn
namedVarPat = noLocValue . VarPat noValue . noLocValue

-- | @IO ()@
ioUnit :: LHsType GhcRn
ioUnit =
    nlHsAppTy
      (nlHsTyVar NotPromoted Names.ioTyConName)
      (nlHsTyVar NotPromoted (tyConName unitTyCon))
