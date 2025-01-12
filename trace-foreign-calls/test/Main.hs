module Main (main) where

import Control.Concurrent.Async (concurrently)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads(..))

import Test.TraceForeignCalls.UsePlugin

main :: IO ()
main = defaultMain $ localOption (NumThreads 1) $
    testGroup "trace-foreign-calls" [
        testCase "answerIO" $ do
          answer <- answerIO
          assertEqual "" 42 $ answer
      , testCase "answerPure" $ do
          let answer = answerPure
          assertEqual "" 42 $ answer
      , testCase "slowAddIO" $ do
          let a = 1_000_000_000
              b = 2_000_000_000
          result <- slowAddIO a b
          assertEqual "" (a + b) $ result
      , testCase "slowAddPure" $ do
          let a = 1_000_000_000
              b = 2_000_000_000
          let result = slowAddPure a b
          assertEqual "" (a + b) $ result
      , testCase "slowAddConcurrent" $ do
          let a = 1_000_000_000
              b = 2_000_000_000
          (ab, ba) <- concurrently (slowAddIO a b) (slowAddIO b a)
          assertEqual "" ab ba
      ]
