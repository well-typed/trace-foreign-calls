module TestA (main) where

import ExamplePkgA

main :: IO ()
main = print =<< someForeignFunInA