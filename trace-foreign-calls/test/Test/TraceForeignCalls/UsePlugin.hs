-- {-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls
--                 -fplugin-opt Plugin.TraceForeignCalls:dump-generated #-}
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls #-}

module Test.TraceForeignCalls.UsePlugin (
    -- * IO functions
    answerIO
  , answerPure
  , slowAddIO
  , slowAddPure
  ) where

import Foreign.C

foreign import capi "test_cbits.h answer"   c_answerIO   :: IO CInt
foreign import capi "test_cbits.h answer"   c_answerPure ::    CInt

foreign import capi "test_cbits.h slow_add" c_slowAddIO   :: CLong -> CLong -> IO CLong
foreign import capi "test_cbits.h slow_add" c_slowAddPure :: CLong -> CLong ->    CLong

answerIO :: IO Int
answerIO = fromIntegral <$> c_answerIO

answerPure :: Int
answerPure = fromIntegral $ c_answerPure

slowAddIO :: Int -> Int -> IO Int
slowAddIO a b = fromIntegral <$> c_slowAddIO (fromIntegral a) (fromIntegral b)

slowAddPure :: Int -> Int -> Int
slowAddPure a b = fromIntegral $ c_slowAddPure (fromIntegral a) (fromIntegral b)
