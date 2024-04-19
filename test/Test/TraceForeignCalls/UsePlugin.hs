-- {-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls -fplugin-opt Plugin.TraceForeignCalls:dump-generated #-}
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls #-}

module Test.TraceForeignCalls.UsePlugin (
    answerIO
  , slowAddIO
  ) where

import Foreign.C

foreign import capi "test_cbits.h answer"   c_answerIO  :: IO CInt
foreign import capi "test_cbits.h slow_add" c_slowAddIO :: CLong -> CLong -> IO CLong

answerIO :: IO Int
answerIO = fromIntegral <$> c_answerIO

slowAddIO :: Int -> Int -> IO Int
slowAddIO a b = fromIntegral <$> c_slowAddIO (fromIntegral a) (fromIntegral b)
