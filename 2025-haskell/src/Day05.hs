{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day05 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.HashMap.Strict (HashMap, empty)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable, hash)
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import GHC.Generics (Generic)
import InputFetcher (getInput)
import Text.Parsec hiding (count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf

data RangeInc = RangeInc {lowBound :: Int, highBound :: Int} deriving (Show, Eq, Ord)

inRange :: RangeInc -> Int -> Bool
inRange r x = x >= lowBound r && x <= highBound r

type Input = ([RangeInc], [Int])

part1 :: Input -> Int
part1 input =
  let (ranges, ingredients) = input
      fresh = filter (\i -> any (`inRange` i) ranges) ingredients
   in length fresh

data MergeState = MergeState {totalSum :: Int, currentStart :: Int, currentEnd :: Int} deriving (Show)

zeroMS = MergeState 0 0 0

mergeRanges :: MergeState -> RangeInc -> MergeState
mergeRanges (MergeState t cs ce) (RangeInc ns ne)
  -- next range starts after current range ends, so we just add the next range normally
  -- and the next state is replaced with the next range
  | ns > ce = MergeState (t + ne - ns + 1) ns ne
  -- next range starts at the same point as current range ends, so we need to subtract 1 for the overlap
  | ns == ce = MergeState (t + ne - ns) ns ne
  -- next range starts earlier than the current range ending, so we need to subtract the full overlap amount
  -- .....................
  -- |---------|
  --        |------|
  -- cs     ns ce  ne
  | ns < ce = MergeState (t + ne - ns - (ce - ns)) ns ne

traceAcc :: (Show a, Show b) => (a -> b -> a) -> (a -> b -> a)
traceAcc f x n =
  let result = f x n
  in traceShow result result

part2 :: Input -> Int
part2 input =
  let (ranges, _) = input
      merged = foldl ( mergeRanges) zeroMS $ sort ranges
   in totalSum merged

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseRange :: Text -> RangeInc
parseRange i =
  let (loT, _) = T.breakOn "-" i
      (_, hiT) = T.breakOnEnd "-" i
   in RangeInc (read $ T.unpack loT) (read $ T.unpack hiT)

parseInput :: Text -> Either String Input
parseInput i =
  let (rangesT, ingredientsT) = T.breakOn "\n\n" i
      rangesL = T.lines $ T.strip rangesT
      ingredientsL = T.lines $ T.strip ingredientsT
      ingredients = map (read . T.unpack) ingredientsL
      ranges = map parseRange rangesL
   in Right $ (ranges, ingredients)

solve :: IO ()
solve = do
  input <- getInput 2025 5
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
