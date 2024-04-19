module ExamplePkgB (someWrapperInB) where

import Codec.Compression.Zlib
import Data.ByteString.Lazy.Char8 qualified as BS.Char8
import Data.ByteString.Lazy qualified as BS.Word8

import ExamplePkgA

someWrapperInB :: IO ()
someWrapperInB = do
    randomNumber <- someForeignFunInA
    let bs = compress (BS.Char8.pack $ show randomNumber)
    print $ BS.Word8.unpack bs
