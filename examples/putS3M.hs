module Main (main) where

import Codec.Tracker.S3M
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

main :: IO ()
main = ((BLC.putStrLn . runPut) . putModule <$> runGet getModule) =<< BL.getContents
