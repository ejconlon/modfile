{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT.Pattern (
      Pattern (..)
    , Cell (..)
    , Command (..)
    , emptyCell
    , getPattern
    , getEmptyPattern
    , putPattern
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Maybe
import           Data.Traversable

import           Util


data Command = Command { cmd :: Word8
                       , val :: Word8
                       }
    deriving (Show, Eq)

getCommand :: Get Command
getCommand = label "IT.Pattern Command" $
             Command <$> getWord8 <*> getWord8

putCommand :: Command -> Put
putCommand Command{..} =
    putWord8 cmd >> putWord8 val

data Cell = Cell { channel    :: Word8
                 , _mask      :: Word8
                 , note       :: Maybe Word8
                 , instrument :: Maybe Word8
                 , volpan     :: Maybe Word8
                 , command    :: Maybe Command
                 }
    deriving (Show, Eq)

emptyCell :: Cell
emptyCell = Cell 0 0 Nothing Nothing Nothing Nothing

getCell :: Word8 -> Word8 -> [Cell] -> Get Cell
getCell channel mask rowBuffer = label "IT.Pattern Cell" $ do
    n <- item 4 0 getWord8 note
    i <- item 5 1 getWord8 instrument
    v <- item 6 2 getWord8 volpan
    c <- item 7 3 getCommand command
    return $ Cell channel mask n i v c
  where lastCell       = rowBuffer !! fromIntegral channel
        item b1 b2 g a = if testBit mask b1
                         then pure (a lastCell)
                         else getByMask mask b2 g

data Pattern = Pattern { length  :: Word16
                       , numRows :: Word16
                       , ppad0   :: [Word8]           -- 4 bytes
                       , rows    :: [[Cell]]
                       }
    deriving (Show, Eq)

getPattern :: Get Pattern
getPattern = label "IT.Pattern" $ do
    length <- getWord16le
    numRows <- getWord16le
    ppad0 <- replicateM 4 getWord8
    rows <- fmap (map fst) (getRows (fromIntegral numRows))
    return Pattern{..}

getEmptyPattern :: Get Pattern
getEmptyPattern = return $ Pattern 0 64 [0, 0, 0, 0] (replicate 64 [])

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord16le length
    putWord16le numRows
    mapM_ putWord8 ppad0
    putRows rows

replaceNth n new (x : xs)
    | n == 0 = new : xs
    | otherwise = x : replaceNth (n - 1) new xs

getRow :: [Cell] -> Get ([Cell], [Cell])
getRow rowBuffer = label "IT.Pattern Row" $ do
    chn <- getWord8
    if chn == 0
        then return ([], rowBuffer)
        else do
            let channel = (chn - 1) .&. 63
            mask <- if testBit chn 7
                    then getWord8
                    else pure (_mask (rowBuffer !! fromIntegral channel))
            c <- getCell channel mask rowBuffer
            j <- getRow (if testBit chn 7
                         then replaceNth (fromIntegral channel) c rowBuffer
                         else rowBuffer)
            return (fst j ++ [c], snd j)

getRows :: Int -> Get [([Cell], [Cell])]
getRows cnt
    | cnt <= 0 = pure [([], replicate 64 emptyCell)]
    | otherwise = do
          x <- getRows (cnt - 1)
          y <- getRow $ snd (last x)
          return $ x ++ [y]

putRows :: [[Cell]] -> Put
putRows _ = fail "not implemented"

