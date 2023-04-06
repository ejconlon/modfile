module Main (main) where

import Codec.Tracker.XM
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  file <- BL.getContents
  let xm0 = runGet getModule file
      xm1 = runGet getModule $ runPut $ putModule xm0
  if xm0 == xm1
    then putStrLn "xm0 and xm1 are identical"
    else putStrLn "xm0 and xm1 differ"
