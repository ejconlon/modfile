{-# LANGUAGE RecordWildCards #-}

module Data.Module.XM.Instrument (
      Instrument (..)
    , getInstrument
    , putInstrument
    ) where

import           Data.Module.XM.Sample

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put

data Instrument = Instrument { instrumentSize :: Word32
                             , name           :: [Word8]    -- 22 bytes
                             , instrumentType :: Word8
                             , sampleNum      :: Word16
-- if numSamples > 0
-- TODO
                             }
    deriving (Show, Eq)

getInstrument :: Get Instrument
getInstrument = Instrument <$> getWord32le <*> replicateM 22 getWord8
                           <*> getWord8 <*> getWord16le
-- TODO

putInstrument :: Instrument -> Put
putInstrument Instrument{..} = do
    putWord32le instrumentSize
    mapM_ putWord8 name
    putWord8 instrumentType
    putWord16le sampleNum
-- TODO

