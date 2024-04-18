module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.TraceForeignCalls.UsePlugin

main :: IO ()
main = defaultMain $ testGroup "trace-foreign-calls" [
      testCase "answer" $ do
        answer <- answerIO
        assertEqual "" 42 answer
    ]
