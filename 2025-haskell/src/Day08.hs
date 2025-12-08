module Day07 (solve, part1, part2, parseInput) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GridRC (GridRC)
import qualified GridRC as G
import InputFetcher (getInput)
import PointRC (PointRC (..))
import qualified PointRC as P

type Input = GridRC

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

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
parseInput = G.parse

solve :: IO ()
solve = do
  input <- getInput 2025 7
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
