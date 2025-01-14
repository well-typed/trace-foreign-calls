module Main (main) where

import Control.Concurrent.Async (concurrently)

import Test.TraceForeignCalls.UsePlugin

main :: IO ()
main = do
    print =<< answerIO
    print $ answerPure
    print =<< slowAddIO a b
    print =<< slowAddIO b a
    print $ slowAddPure a b
    print $ slowAddPure b a
    print =<< concurrently (slowAddIO a b) (slowAddIO b a)
    print =<< concurrently (slowAddIO b a) (slowAddIO a b)
  where
    a = 1_000_000_000
    b = 2_000_000_000
