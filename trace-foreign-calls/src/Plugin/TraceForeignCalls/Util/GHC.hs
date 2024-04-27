{-# LANGUAGE OverloadedStrings #-}

module Plugin.TraceForeignCalls.Util.GHC (
    -- * Access to 'HscEnv'
    HasHscEnv(..)
    -- * Errors and warnings
  , throwSimpleError
  , printSimpleWarning
    -- * Names
  , resolveVarName
  , resolveTcName
  ) where

import GHC hiding (getNamePprCtx)
import GHC.Plugins hiding (getNamePprCtx, getHscEnv)

import GHC.Data.IOEnv
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Rename.Env
import GHC.Runtime.Context
import GHC.Tc.Types
import GHC.Types.Error
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
        (defaultDiagnosticOpts @DiagnosticMessage)
        diagOpts
        (singleMessage $ mkMsgEnvelope diagOpts l namePprCtx diag)
  where
    diag :: DiagnosticMessage
    diag = DiagnosticMessage {
      diagMessage = mkSimpleDecorated doc
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
