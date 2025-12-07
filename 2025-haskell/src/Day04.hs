module Day04 (solve, part1, part2, parseInput) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Debug.Trace
import GridRC (GridRC)
import qualified GridRC as G
import InputFetcher (getInput)
import PointRC (PointRC (..))
import qualified PointRC as P

type Input = GridRC

countNeighbors :: GridRC -> PointRC -> Int
countNeighbors input p =
  let neighborPoints = P.neighbor8 p
      neighborValues = map (\n -> fromMaybe '.' $ G.get n input) neighborPoints
      boxes = filter (== '@') neighborValues
   in length boxes

part1 :: Input -> Int
part1 input =
  let rolls = G.findMany '@' input
      counts = map (countNeighbors input) rolls
      moveable = filter (< 4) counts
   in -- trace (show (filter (<4) counts))
      length moveable

part2 :: Input -> Int
part2 input = go input
  where
    go :: GridRC -> Int
    go currentMap =
      let rolls = G.findMany '@' currentMap
          counts = map (\r -> (r, countNeighbors currentMap r)) rolls
          moveable = map fst $ filter ((< 4) . snd) counts
          nextMap = G.rewrite (\p c -> if elem p moveable then '.' else c) currentMap
          movedInRemainder = go nextMap
       in -- traceShow (rolls, currentMap)
          (if null moveable then 0 else movedInRemainder + length moveable)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput = G.parse

solve :: IO ()
solve = do
  input <- getInput 2025 4
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
