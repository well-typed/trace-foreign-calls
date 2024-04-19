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
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Debug.Trace qualified
import qualified Language.Haskell.TH.Syntax as TH

import GHC
import GHC.Plugins

import GHC.Driver.Hooks
import GHC.Tc.Types
import GHC.Types.TyThing
import GHC.Utils.Logger

import Plugin.TraceForeignCalls.Options
import Plugin.TraceForeignCalls.Util.GHC

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
      -- exceptions
    , MonadThrow
    , MonadCatch
    , MonadMask
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
    tracerEnvNames   <- initNames

    let tracerEnv :: TracerEnv
        tracerEnv = TracerEnv {
              tracerEnvOptions
            , tracerEnvNames
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
-------------------------------------------------------------------------------}

data Names = Names {
      nameTraceEventIO :: Name
    }

initNames :: TcM Names
initNames = do
    nameTraceEventIO <- resolveTHName (TH.mkNameG TH.VarName "ghc-internal" "GHC.Internal.Debug.Trace" "traceEventIO")
    return Names {
        nameTraceEventIO
      }

findName :: (Names -> a) -> Instrument a
findName f = Wrap $ asks (f . tracerEnvNames)
