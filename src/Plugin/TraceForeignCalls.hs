module Plugin.TraceForeignCalls (plugin) where

import GHC.Plugins

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      parsedResultAction = processParsed
    }

processParsed ::
     [CommandLineOption]
  -> ModSummary
  -> ParsedResult -> Hsc ParsedResult
processParsed _ _ = return
