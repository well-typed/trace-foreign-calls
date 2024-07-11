-- | Instrumentation monad
--
-- Intended for unqualified import.
module Plugin.TraceForeignCalls.Instrument (
    Instrument -- opaque
  , runInstrument
  , liftTcM
    -- * Options
  , asksOption
  , whenOption
  , whenOption_
    -- * Names
  , Names(..)
  , findName
  ) where

import Control.Monad
import Control.Monad.IO.Class

import GHC
import GHC.Plugins hiding (getHscEnv)

import GHC.Tc.Types
import GHC.Utils.Logger

import Plugin.TraceForeignCalls.Options
import Plugin.TraceForeignCalls.Util.GHC
import Plugin.TraceForeignCalls.Util.Shim

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TracerEnv = TracerEnv {
      tracerEnvOptions :: Options
    , tracerEnvNames   :: Names
    }

newtype Instrument a = Wrap { unwrap :: TracerEnv -> TcM a }

liftTcM :: TcM a -> Instrument a
liftTcM = Wrap . const

getTracerEnv :: Instrument TracerEnv
getTracerEnv = Wrap return

runInstrument :: forall a. Bool -> [String] -> Instrument a -> TcM a
runInstrument profiled rawOptions ma = do
    tracerEnvOptions' <- parseOptions rawOptions
    let tracerEnvOptions = tracerEnvOptions' { optionsDisableCallStack = not profiled || optionsDisableCallStack tracerEnvOptions' }

    let tracerEnv :: TracerEnv
        tracerEnv = TracerEnv {
              tracerEnvOptions
            , tracerEnvNames = mkNames
            }

    unwrap ma tracerEnv

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Functor Instrument where
  fmap = liftM

instance Applicative Instrument where
  pure x = Wrap $ \_env -> return x
  (<*>)  = ap

instance Monad Instrument where
  x >>= f = Wrap $ \env -> unwrap x env >>= \a -> unwrap (f a) env

instance MonadIO Instrument where
  liftIO = liftTcM  . liftIO

instance HasDynFlags Instrument where getDynFlags      = liftTcM getDynFlags
instance HasHscEnv   Instrument where getHscEnv        = liftTcM getHscEnv
instance HasLogger   Instrument where getLogger        = liftTcM getLogger
instance MonadUnique Instrument where getUniqueSupplyM = liftTcM getUniqueSupplyM

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

asksOption :: (Options -> a) -> Instrument a
asksOption f = f . tracerEnvOptions <$> getTracerEnv

whenOption :: (Options -> Bool) -> Instrument a -> Instrument (Maybe a)
whenOption f ma = do
    flag <- asksOption f
    if flag
      then Just <$> ma
      else return Nothing

whenOption_ :: (Options -> Bool) -> Instrument () -> Instrument ()
whenOption_ f = void . whenOption f

{-------------------------------------------------------------------------------
  Names

  We set things up in such a way that we only try to resolve a name when we
  actually use it. We could add some caching, but it really doesn't matter.
-------------------------------------------------------------------------------}

data Names = Names {
      nameTraceEventHash :: TcM Name
    , nameSeq            :: TcM Name
    , nameRunRW          :: TcM Name
    , nameNoDuplicate    :: TcM Name
    , nameHasCallStack    :: TcM Name
    , nameCallStack       :: TcM Name
    , namePrettyCallStack :: TcM Name
    , nameGetCurrentCCS   :: TcM Name
    }

mkNames :: Names
mkNames = Names {
      nameTraceEventHash  = resolveVarName modlTraceEvent      "traceEvent#"
    , nameSeq             = resolveVarName modlSeq             "seq#"
    , nameRunRW           = resolveVarName modlRunRW           "runRW#"
    , nameNoDuplicate     = resolveVarName modlNoDuplicate     "noDuplicate#"
    , nameHasCallStack    = resolveTcName  modlHasCallStack    "HasCallStack"
    , nameCallStack       = resolveVarName modlCallStack       "callStack"
    , namePrettyCallStack = resolveVarName modlPrettyCallStack "prettyCallStack"
    , nameGetCurrentCCS   = resolveVarName modlGetCurrentCCS   "getCurrentCCS#"
    }

findName :: (Names -> TcM Name) -> Instrument Name
findName f = Wrap $ f . tracerEnvNames
