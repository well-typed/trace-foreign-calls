-- | Instrumentation monad
--
-- Intended for unqualified import.
module Plugin.TraceForeignCalls.Instrument (
    Instrument -- opaque
  , runInstrument
  , liftTcM
  , getOptions
    -- * Names
  , Names(..)
  , findName
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (isJust)

import GHC
import GHC.Plugins hiding (getHscEnv)

import GHC.Builtin.Names qualified as Names
import GHC.Platform.Ways (Way(WayProf), hasWay)
import GHC.Tc.Utils.Monad (TcM, TcGblEnv)
import GHC.Tc.Utils.Monad qualified as TC
import GHC.Unit.Env (lookupHugByModule)
import GHC.Utils.Logger (HasLogger(..))

import Plugin.TraceForeignCalls.Options
import Plugin.TraceForeignCalls.Util.GHC

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

runInstrument :: TcGblEnv -> [String] -> Instrument a -> TcM a
runInstrument tcGblEnv rawOptions ma = do
    tracerEnvOptions <- parseOptions rawOptions
    tracerEnvNames   <- mkNames tcGblEnv

    let tracerEnv :: TracerEnv
        tracerEnv = TracerEnv{
              tracerEnvOptions
            , tracerEnvNames
            }
    unwrap ma tracerEnv

getOptions :: Instrument Options
getOptions = tracerEnvOptions <$> getTracerEnv

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
  Names

  We set things up in such a way that we only try to resolve a name when we
  actually use it. We could add some caching, but it really doesn't matter.
-------------------------------------------------------------------------------}

data Names = Names {
      -- | @ghc-prim:GHC.Prim.getCurrentCCS#@
      nameGetCurrentCCS :: Name

      -- | @ghc-prim:GHC.Prim.noDuplicate#@
    , nameNoDuplicate :: Name

      -- | @ghc-prim:GHC.Prim.traceEvent#@
    , nameTraceEvent :: Name

      -- | @ghc-prim:GHC.Prim.myThreadId#@
    , nameMyThreadId :: Name

      -- | @ghc-prim:GHC.Prim.threadStatus#@
    , nameThreadStatus :: Name

      -- | @ghc-prim:GHC.Prim.intToInt64#@
    , nameIntToInt64 :: Name

      -- | @ghc-prim:GHC.Prim.int64ToWord64#@
    , nameInt64ToWord64 :: Name

      -- | @ghc-prim:GHC.Magic.runRW#@
    , nameRunRW :: Name

      -- | @ghc-internal:GHC.Internal.IO.seq#@
      --
      -- NOTE: This is only available /after/ @GHC.Internal.IO@ is compiled.
      -- For this reason we cannot add any tracing to pure FFI calls in
      -- @GHC.Internal.IO@ (currently there aren't any).
    , nameSeq :: Maybe Name

      -- | @traceCCS#@
      --
      -- This name is a little different from the others: it does not exist
      -- as a standard (foreign) import anywhere, and so we need to import it
      -- ourselves. To avoid name clashes, we generate a new unique name for it.
      --
      -- 'Nothing' if we are not in profiling mode
    , nameTraceCCS :: Maybe Name
    }

mkNames :: TcGblEnv -> TcM Names
mkNames tcGblEnv = do
    nameGetCurrentCCS <- var prim "GHC.Prim"  "getCurrentCCS#"
    nameNoDuplicate   <- var prim "GHC.Prim"  "noDuplicate#"
    nameTraceEvent    <- var prim "GHC.Prim"  "traceEvent#"
    nameMyThreadId    <- var prim "GHC.Prim"  "myThreadId#"
    nameThreadStatus  <- var prim "GHC.Prim"  "threadStatus#"
    nameIntToInt64    <- var prim "GHC.Prim"  "intToInt64#"
    nameInt64ToWord64 <- var prim "GHC.Prim"  "int64ToWord64#"
    nameRunRW         <- var prim "GHC.Magic" "runRW#"

    haveSeq <- checkHaveSeq tcGblEnv <$> TC.getTopEnv
    nameSeq <- if haveSeq
                 then Just <$> var intr "GHC.Internal.IO" "seq#"
                 else return Nothing

    profiling    <- checkProfiling <$> getDynFlags
    nameTraceCCS <- if profiling
                      then Just <$> uniqInternalName "traceCCS#"
                      else return Nothing

    return Names {
        nameGetCurrentCCS
      , nameNoDuplicate
      , nameRunRW
      , nameTraceEvent
      , nameMyThreadId
      , nameThreadStatus
      , nameIntToInt64
      , nameInt64ToWord64
      , nameSeq
      , nameTraceCCS
      }
  where
    var :: Unit -> String -> String -> TcM Name
    var pkg modl fn = resolveVarName (mkModule pkg $ mkModuleName modl) fn

    prim, intr :: Unit
    prim = primUnit
    intr = ghcInternalUnit

findName :: (Names -> a) -> Instrument a
findName f = Wrap $ return . f . tracerEnvNames

checkHaveSeq :: TcGblEnv -> HscEnv -> Bool
checkHaveSeq tcGblEnv hsc =
    if moduleUnit (TC.tcg_mod tcGblEnv) == ghcInternalUnit
      then isJust $ lookupHugByModule Names.gHC_INTERNAL_IO (hsc_HUG hsc)
      else True

checkProfiling :: DynFlags -> Bool
checkProfiling df = sccProfilingEnabled df && ways df `hasWay` WayProf
