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

      -- | Debugging (of the plugin itself)
    , optionsDebug :: Bool
    }

defaultOptions :: Options
defaultOptions = Options {
      optionsDumpGenerated = False
    , optionsDebug         = False
    }

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseOptions :: forall m. HasHscEnv m => [String] -> m Options
parseOptions = ($ defaultOptions) . foldr (>=>) return . map aux
  where
    aux :: String -> Options -> m Options
    aux "dump-generated" opts = return $ opts { optionsDumpGenerated = True }
    aux "debug"          opts = return $ opts { optionsDebug         = True }
    aux opt              _    = throwSimpleError noSrcSpan $ hcat [
                                    "Unexpected option "
                                  , fromString (show opt)
                                  ]

