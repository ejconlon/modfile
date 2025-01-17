{-# LANGUAGE RecordWildCards #-}

-- | read/write Scream Tracker 3 Instruments
module Codec.Tracker.S3M.Instrument
  ( Instrument (..)
  , getInstrument
  , putInstrument
  )
where

import Codec.Tracker.S3M.Instrument.Adlib
import Codec.Tracker.S3M.Instrument.PCM
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Maybe

-- | Scream Tracker 3 instrument
data Instrument = Instrument
  { instrumentType :: Word8
  -- ^ 0: empty, 1: PCM, 2: adlib melody instrument, 3-7: adlib percussion instrument
  , fileName :: [Word8]
  -- ^ 12 bytes
  , pcmSample :: Maybe PCMSample
  -- ^ if instrumentType == 1
  , adlibSample :: Maybe AdlibSample
  }
  deriving (Show, Eq)

-- | Read an `Instrument` from the monad state.
getInstrument :: Get Instrument
getInstrument = label "S3M.Instrument" $ do
  instrumentType <- getWord8
  fileName <- replicateM 12 getWord8
  pcmSample <- sequence $ if instrumentType == 1 then Just getPCMSample else Nothing
  adlibSample <- sequence $ if instrumentType `elem` [2 .. 7] then Just getAdlibSample else Nothing
  return Instrument {..}

-- | Write an `Instrument` to the buffer.
putInstrument :: Int -> Instrument -> Put
putInstrument ptr Instrument {..} = do
  putWord8 instrumentType
  mapM_ putWord8 fileName
  when (isNothing pcmSample && isNothing adlibSample) $ do
    mapM_ putWord8 $ replicate 63 0 -- padding because of reasons
    putWord32le 0x53524353
  forM_ pcmSample $ putPCMSample ptr
  forM_ adlibSample putAdlibSample
