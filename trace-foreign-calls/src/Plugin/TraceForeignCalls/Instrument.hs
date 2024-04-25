{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import GHC
import GHC.Plugins

import GHC.Driver.Hooks
import GHC.Tc.Types
import GHC.Types.TyThing
import GHC.Utils.Logger

import Plugin.TraceForeignCalls.Options
import Plugin.TraceForeignCalls.Util.GHC

-- For name resolution
import Debug.Trace qualified
import Control.Exception qualified
import System.IO.Unsafe qualified
import GHC.Stack qualified

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TracerEnv = TracerEnv {
      tracerEnvOptions :: Options
    , tracerEnvNames   :: Names
    }

newtype Instrument a = Wrap { unwrap :: ReaderT TracerEnv TcM a }
  deriving newtype
    ( -- base
      Functor
    , Applicative
    , Monad
    , MonadIO
      -- ghc
    , HasDynFlags
    , MonadThings
      -- trace-foreign-calls
    , HasHscEnv
    )

liftTcM :: TcM a -> Instrument a
liftTcM = Wrap . lift

runInstrument :: forall a. [String] -> Instrument a -> TcM a
runInstrument rawOptions ma = do
    tracerEnvOptions <- parseOptions rawOptions

    let tracerEnv :: TracerEnv
        tracerEnv = TracerEnv {
              tracerEnvOptions
            , tracerEnvNames = mkNames
            }

    runReaderT (unwrap ma) tracerEnv

{-------------------------------------------------------------------------------
  Manual instances

  Unfortunately many classes in GHC do not provide instances for transformers.
-------------------------------------------------------------------------------}

instance HasHooks    Instrument where getHooks         = liftTcM getHooks
instance HasModule   Instrument where getModule        = liftTcM getModule
instance HasLogger   Instrument where getLogger        = liftTcM getLogger
instance MonadUnique Instrument where getUniqueSupplyM = liftTcM getUniqueSupplyM

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

asksOption :: (Options -> a) -> Instrument a
asksOption f = Wrap $ asks (f . tracerEnvOptions)

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
      nameTraceEventIO    :: TcM Name
    , nameEvaluate        :: TcM Name
    , nameUnsafePerformIO :: TcM Name
    , nameHasCallStack    :: TcM Name
    , nameCallStack       :: TcM Name
    , namePrettyCallStack :: TcM Name
    }

mkNames :: Names
mkNames = Names {
      nameTraceEventIO    = resolveTHName 'Debug.Trace.traceEventIO
    , nameEvaluate        = resolveTHName 'Control.Exception.evaluate
    , nameUnsafePerformIO = resolveTHName 'System.IO.Unsafe.unsafePerformIO
    , nameHasCallStack    = resolveTHName ''GHC.Stack.HasCallStack
    , nameCallStack       = resolveTHName 'GHC.Stack.callStack
    , namePrettyCallStack = resolveTHName 'GHC.Stack.prettyCallStack
    }

findName :: (Names -> TcM Name) -> Instrument Name
findName f = Wrap $ ReaderT $ f . tracerEnvNames
