module Main (main) where

import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Char8   as BLC
import           Data.Binary.Get
import           Data.Binary.Put

import           Codec.Tracker.S3M

main :: IO ()
main = BLC.putStrLn . runPut <$> putModule <$> runGet getModule =<< BL.getContents

