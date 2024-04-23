module ExamplePkgB (someWrapperInB) where

import ExamplePkgA

someWrapperInB :: IO Int
someWrapperInB = fromIntegral <$> someForeignFunInA