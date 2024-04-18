module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.TraceForeignCalls.UsePlugin

main :: IO ()
main = defaultMain $ testGroup "trace-foreign-calls" [
      testCase "answerIO" $ do
        answer <- answerIO
        assertEqual "" 42 $ answer
    , testCase "slowAddIO" $ do
        let a = 1_000_000_000
            b = 2_000_000_000
        result <- slowAddIO a b
        assertEqual "" (a + b) $ result
    ]
