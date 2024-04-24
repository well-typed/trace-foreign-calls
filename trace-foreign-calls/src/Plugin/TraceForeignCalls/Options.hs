{-# LANGUAGE OverloadedStrings #-}

module Plugin.TraceForeignCalls.Options (
    Options(..)
  , defaultOptions
  , parseOptions
  ) where

import Control.Monad
import Data.String

import GHC
import GHC.Utils.Outputable

import Plugin.TraceForeignCalls.Util.GHC

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Options = Options {
      -- | Dump the generated code
      optionsDumpGenerated :: Bool

      -- | Disable generating HasCallStack constraints
      --
      -- By default the generated wrappers have a 'HasCallStack' constraint,
      -- which is used to add additional info into the eventlog. For some
      -- applications however this may cause problems.
    , optionsDisableCallStack :: Bool
    }

defaultOptions :: Options
defaultOptions = Options {
      optionsDumpGenerated    = False
    , optionsDisableCallStack = False
    }

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseOptions :: forall m. HasHscEnv m => [String] -> m Options
parseOptions = ($ defaultOptions) . foldr (>=>) return . map aux
  where
    aux :: String -> Options -> m Options
    aux "dump-generated"    opts = return $ opts { optionsDumpGenerated    = True }
    aux "disable-callstack" opts = return $ opts { optionsDisableCallStack = True }
    aux opt                 _    = throwSimpleError noSrcSpan $ hcat [
                                       "Unexpected option "
                                     , fromString (show opt)
                                     ]

