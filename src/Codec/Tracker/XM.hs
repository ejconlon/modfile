{-# LANGUAGE RecordWildCards #-}

-- | read/write Fasttracker II files
module Codec.Tracker.XM
  ( Module (..)
  , getModule
  , putModule
  )
where

import Codec.Tracker.XM.Header
import Codec.Tracker.XM.Instrument
import Codec.Tracker.XM.Pattern
import Control.Monad
import Data.Binary
import Data.Binary.Get

-- | A Fasttracker II module
data Module = Module
  { header :: Header
  , orders :: [Word8]
  , patterns :: [Pattern]
  , instruments :: [Instrument]
  }
  deriving (Show, Eq)

-- | Read a `Module` from the monad state.
getModule :: Get Module
getModule = label "XM" $ do
  header <- getHeader
  orders <- replicateM (fromIntegral (songLength header)) getWord8
  br <- bytesRead
  skip $ 60 + fromIntegral (headerSize header) - fromIntegral br
  patterns <- replicateM (fromIntegral (numPatterns header)) getPattern
  instruments <- replicateM (fromIntegral (numInstruments header)) getInstrument
  return Module {..}

-- | Write a `Module` to the buffer.
putModule :: Module -> Put
putModule Module {..} = do
  putHeader header
  mapM_ putWord8 orders
  replicateM_ (fromIntegral (headerSize header) - 20 - length orders) (putWord8 0)
  mapM_ putPattern patterns
  mapM_ putInstrument instruments
