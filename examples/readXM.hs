{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

import           Codec.Tracker.XM
import           Codec.Tracker.XM.Header
import           Codec.Tracker.XM.Instrument
import           Codec.Tracker.XM.Pattern

pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    BL.putStr $ BL.pack instrumentName
    putStrLn $ " (" ++ show sampleNum ++ ")"

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr     "Song name.......: "
    BL.putStrLn $ BL.pack songName
    putStr     "Tracker name....: "
    BL.putStrLn $ BL.pack trackerName
    putStrLn $ "Version.........: " ++ show version
    putStrLn $ "Orders..........: " ++ show songLength
    putStrLn $ "Restart position: " ++ show restartPosition
    putStrLn $ "Instruments.....: " ++ show numInstruments
    putStrLn $ "Channels........: " ++ show numChannels
    putStrLn $ "Patterns........: " ++ show numPatterns
    putStrLn $ "Initial speed...: " ++ show initialSpeed
    putStrLn $ "Initial tempo...: " ++ show initialTempo

pprintPattern :: Pattern -> IO ()
pprintPattern Pattern{..} = do
    putStrLn $ "Packed size: " ++ show packedSize ++ "  Rows: " ++ show numRows
    print patternData

main :: IO ()
main = do
    file <- BL.getContents
    let xm = runGet getModule file
    putStrLn "Header:"
    putStrLn "======="
    pprintHeader $ header xm
    putStrLn "<>"
    print (orders xm)
    putStrLn "<>"
    putStrLn "Instruments:"
    putStrLn "============"
    mapM_ pprintInstrument (instruments xm)
    putStrLn "<>"
    putStrLn "Patterns:"
    putStrLn "========="
    mapM_ pprintPattern (patterns xm)
    putStrLn "<>"
