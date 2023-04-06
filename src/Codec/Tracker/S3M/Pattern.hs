{-# LANGUAGE RecordWildCards #-}

-- | read/write Scream Tracker 3 patterns
module Codec.Tracker.S3M.Pattern
  ( Pattern (..)
  , Command (..)
  , Cell (..)
  , Note (..)
  , channel
  , packedSize
  , emptyCell
  , getPattern
  , putPattern
  )
where

import Codec.Tracker.Common
import Control.Applicative ((<$>))
import Control.Monad.Loops
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Maybe
import Text.Printf
import Util

-- | Song event
data Cell = Cell
  { mask :: Word8
  , note :: Maybe Note
  , instrument :: Maybe Word8
  , volume :: Maybe Word8
  , command :: Maybe Command
  }
  deriving (Eq)

instance Enum Note where
  toEnum n
    | n < 254 = Note $ toEnum n
    | n == 254 = NoteCut
    | n == 255 = NoteOff
  fromEnum (Note p) = fromEnum p
  fromEnum NoteCut = 254
  fromEnum NoteOff = 255

instance Show Cell where
  show Cell {..} =
    maybe "---" show note
      ++ " "
      ++ maybe ".." (printf "%02X") instrument
      ++ " "
      ++ maybe ".." (printf "%02X") volume
      ++ " "
      ++ maybe "..." show command

-- | channel id
channel :: Cell -> Word8
channel = flip (foldl clearBit . mask) [5 .. 7]

packedSize :: Pattern -> Int
packedSize (Pattern rows) = 2 + length rows + sum (fmap (foldr ((+) . cellSize) 0) rows)
 where
  cellSize Cell {..} = 1 + wj note 1 + wj instrument 1 + wj volume 1 + wj command 2
  wj x n = if isJust x then n else 0

-- | Scream Tracker 3 pattern
data Pattern = Pattern {rows :: [[Cell]]}
  deriving (Show, Eq)

-- | Effect command type
data Command = Command
  { cmd :: Word8
  -- ^ command type
  , val :: Word8
  -- ^ command parameter
  }
  deriving (Eq)

instance Show Command where
  show Command {..} = printf "%1X%02x" cmd val

-- | Read a `Command` from the monad state.
getCommand :: Get Command
getCommand =
  label "S3M.Pattern Command" $
    Command <$> getWord8 <*> getWord8

-- | Write a `Command` to the buffer.
putCommand :: Command -> Put
putCommand Command {..} =
  putWord8 cmd >> putWord8 val

-- | An empty `Cell`
emptyCell :: Cell
emptyCell = Cell 0 Nothing Nothing Nothing Nothing

-- | Read a `Cell` from the monad state.
getCell :: Get (Maybe Cell)
getCell =
  label "S3M.Pattern Cell" $
    getWord8
      >>= \mask ->
        if mask == 0
          then return Nothing
          else do
            n <- getByMask mask 5 (toEnum . fromIntegral <$> getWord8)
            i <- getByMask mask 5 getWord8
            v <- getByMask mask 6 getWord8
            c <- getByMask mask 7 getCommand
            return $ Just (Cell mask n i v c)

-- | Write a `Cell` to the buffer.
putCell :: Maybe Cell -> Put
putCell Nothing = putWord8 0
putCell (Just Cell {..}) =
  putWord8 mask
    >> maybe (return ()) (putWord8 . toEnum . fromEnum) note
    >> maybe (return ()) putWord8 instrument
    >> maybe (return ()) putWord8 volume
    >> maybe (return ()) putCommand command

-- | Read a `Pattern` from the monad state.
getPattern :: Get Pattern
getPattern = label "S3M.Pattern" $ do
  packedLength <- getWord16le
  rows <- getToLimit getRow (packedLength - 2)
  return Pattern {..}

-- | Read a row (a list of `Cell`s) from the monad state.
getRow :: Get [Cell]
getRow = label "S3M.Pattern Row" $ whileJust getCell return

-- | Write a row to the buffer.
putRow :: [Cell] -> Put
putRow l = mapM_ (putCell . Just) l >> putCell Nothing

-- | Write a `Pattern` to the buffer.
putPattern :: Pattern -> Put
putPattern Pattern {..} = putWord16le (fromIntegral $ packedSize Pattern {..}) >> mapM_ putRow rows
