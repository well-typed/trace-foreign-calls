{-# LANGUAGE OverloadedStrings #-}

module Plugin.TraceForeignCalls.Util.GHC (
    -- * Access to 'HscEnv'
    HasHscEnv(..)
    -- * Errors and warnings
  , throwSimpleError
  , printSimpleWarning
    -- * Names
  , resolveTHName
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.String
import Language.Haskell.TH qualified as TH

import GHC hiding (getNamePprCtx)
import GHC.Plugins hiding (getNamePprCtx, getHscEnv)

import GHC.Data.IOEnv
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Runtime.Context
import GHC.Tc.Types
import GHC.Types.Error
import GHC.Types.Name.Cache
import GHC.Utils.Error
import GHC.Utils.Logger

{-------------------------------------------------------------------------------
  Access to 'HscEnv'
-------------------------------------------------------------------------------}

class (MonadIO m, HasDynFlags m) => HasHscEnv m where
  getHscEnv :: m HscEnv

instance HasHscEnv TcM where
  getHscEnv = env_top <$> getEnv

instance HasHscEnv m => HasHscEnv (ReaderT r m) where
  getHscEnv = lift getHscEnv

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

getNameCache :: HasHscEnv m => m NameCache
getNameCache = hsc_NC <$> getHscEnv

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
-------------------------------------------------------------------------------}

resolveTHName :: HasHscEnv m => TH.Name -> m Name
resolveTHName name = do
    nameCache <- getNameCache
    mResolved <- liftIO $ thNameToGhcNameIO nameCache name
    case mResolved of
      Just name' ->
        return name'
      Nothing ->
        throwSimpleError noSrcSpan $ hcat [
            "Could not resolve TH name "
          , fromString $ show name
          ]


