{-# LANGUAGE CApiFFI #-}

module ExamplePkgA (someForeignFunInA) where

import Foreign.C

foreign import capi "cbits.h xkcdRandomNumber" someForeignFunInA :: IO CInt
