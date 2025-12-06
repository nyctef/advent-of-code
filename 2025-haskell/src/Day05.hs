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

data RangeInc = RangeInc { lowBound :: Int, highBound :: Int } deriving (Show)

inRange :: RangeInc -> Int -> Bool
inRange r x = x >= lowBound r && x <= highBound r

type Input = ([RangeInc], [Int])

part1 :: Input -> Int
part1 input = let
    (ranges, ingredients) = input
    fresh = filter (\i -> any (`inRange` i) ranges) ingredients
  in length fresh

part2 :: Input -> Int
part2 input = 0

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseRange :: Text -> RangeInc
parseRange i = let
    (loT, _) = T.breakOn "-" i
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
