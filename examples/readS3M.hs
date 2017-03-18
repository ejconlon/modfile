{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary.Get
import           Data.Maybe
import qualified Data.ByteString.Lazy as BL

import           Data.Module.S3M
import           Data.Module.S3M.Header
import           Data.Module.S3M.Instrument
import           Data.Module.S3M.Instrument.Adlib
import           Data.Module.S3M.Instrument.PCM
import           Data.Module.S3M.Pattern


pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    BL.putStrLn $ BL.pack fileName
    forM_ pcmSample pprintPCMSample
    forM_ adlibSample pprintAdlibSample

pprintAdlibSample :: AdlibSample -> IO ()
pprintAdlibSample AdlibSample{..} = do
    putStr "Adlib: "
    BL.putStrLn $ BL.pack title

pprintPCMSample :: PCMSample -> IO ()
pprintPCMSample PCMSample{..} = do
   putStrLn "PCM: "
   BL.putStrLn $ BL.pack title

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr     "Song name.......: "
    BL.putStrLn $ BL.pack songName
    putStrLn $ "Orders..........: " ++ show songLength
    putStrLn $ "Instruments.....: " ++ show numInstruments
    putStrLn $ "Patterns........: " ++ show numPatterns
    putStrLn $ "Version.........: " ++ show trackerVersion
    putStrLn $ "Global volume...: " ++ show globalVolume
    putStrLn $ "Initial speed...: " ++ show initialSpeed
    putStrLn $ "Initial tempo...: " ++ show initialTempo
    putStrLn $ "Mix volume......: " ++ show mixVolume
    putStrLn $ "Channel settings: " ++ show channelSettings

pprintPattern :: Pattern -> IO ()
pprintPattern Pattern{..} = do
    putStrLn $ "Packed length: " ++ show packedLength
-- TODO

main :: IO ()
main = do
    file <- BL.getContents
    let s3m = runGet getModule file
    putStrLn "Header:"
    putStrLn "======="
    pprintHeader $ header s3m
    putStrLn "<>"
    print (orders s3m)
    putStrLn "<>"
    putStrLn "Instruments:"
    putStrLn "============"
    mapM_ pprintInstrument (instruments s3m)
    
