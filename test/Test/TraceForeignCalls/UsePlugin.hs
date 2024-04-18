{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls #-}

module Test.TraceForeignCalls.UsePlugin (
    answerIO
  ) where

import Foreign.C

foreign import capi "answer.h answer" c_answerIO :: IO CInt

answerIO :: IO Int
answerIO = fromIntegral <$> c_answerIO