module Main (main) where

import Codec.Tracker.IT
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  file <- BL.getContents
  let it0 = runGet getModule file
      it1 = runGet getModule $ runPut $ putModule it0
  if it0 == it1
    then putStrLn "it0 and it1 are identical"
    else putStrLn "it0 and it1 differ"
