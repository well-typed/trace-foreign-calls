module TestB (main) where

import ExamplePkgB

main :: IO ()
main = print =<< someWrapperInB