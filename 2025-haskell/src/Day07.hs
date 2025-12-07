{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day07 (solve, part1, part2, parseInput) where

import Data.Char (isDigit)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import InputFetcher (getInput)
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap, empty)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable, hash)
import Text.Printf

data PointRC = PointRC {row :: Int, col :: Int} deriving (Eq, Generic, Hashable)

instance Show PointRC where
  show p = printf "(r%d c%d)" (row p) (col p)

-- instance Hashable PointRC where
-- hashWithSalt s (PointRC r c) = s + (hash r) + (hash c)
data GridRC = GridRC {grid :: HashMap PointRC Char, numCols :: Int, numRows :: Int} deriving (Show)

type Input = GridRC

cells :: GridRC -> [PointRC]
cells g = [PointRC r c | r <- [0 .. numRows g], c <- [0 .. numCols g]]


part1 :: Input -> Int
part1 input = result
  where
    result = 0


part2 :: Input -> Int
part2 input = result
  where
    result = 0

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i =
  let lines = T.lines i
      cols = map (zip [0 ..] . T.unpack) lines
      rows = zip [0 ..] cols
      cells = [(PointRC r c, val) | (r, cs) <- rows, (c, val) <- cs]
      hashmap = HashMap.fromList cells
   in Right $ GridRC hashmap (length cols) (length rows)

solve :: IO ()
solve = do
  input <- getInput 2025 7
  let parsed1 = parseInput input
  let parsed2 = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed1)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed2)
