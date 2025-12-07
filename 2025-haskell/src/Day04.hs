{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day04 (solve, part1, part2, parseInput) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import InputFetcher (getInput)
import Text.Printf

data PointRC = PointRC {row :: Int, col :: Int} deriving (Eq, Generic, Hashable)

instance Show PointRC where
  show p = printf "(r%d c%d)" (row p) (col p)

-- instance Hashable PointRC where
-- hashWithSalt s (PointRC r c) = s + (hash r) + (hash c)
data GridRC = GridRC {grid :: HashMap PointRC Char, numCols :: Int, numRows :: Int} deriving (Show)

type Input = GridRC

neighbor8 :: PointRC -> [PointRC]
neighbor8 (PointRC r c) =
  [ PointRC (r - 1) (c - 1),
    PointRC (r - 1) c,
    PointRC (r - 1) (c + 1),
    PointRC r (c - 1),
    -- PointRC (r) c,
    PointRC r (c + 1),
    PointRC (r + 1) (c - 1),
    PointRC (r + 1) c,
    PointRC (r + 1) (c + 1)
  ]

-- countNeighbors :: GridRC -> PointRC -> Int
-- countNeighbors g p =
--   let hashmap = grid g
--       neighborPoints = neighbor8 p
--       neighborValues = map (\p -> HashMap.findWithDefault '.' p hashmap) neighborPoints
--       boxes = filter (== '@') neighborValues
--    in length boxes
countNeighbors :: HashMap PointRC Char -> PointRC -> Int
countNeighbors hashmap p =
  let neighborPoints = neighbor8 p
      neighborValues = map (\n -> HashMap.findWithDefault '.' n hashmap) neighborPoints
      boxes = filter (== '@') neighborValues
   in length boxes

part1 :: Input -> Int
part1 input =
  let points = HashMap.toList (grid input)
      rolls = map fst $ filter (\p -> snd p == '@') points
      counts = map (countNeighbors (grid input)) rolls
      moveable = filter (< 4) counts
   in -- trace (show (filter (<4) counts))
      length moveable

part2 :: Input -> Int
part2 input = go (grid input)
  where
    go :: HashMap PointRC Char -> Int
    go currentMap =
      let points = HashMap.toList currentMap
          rolls = map fst $ filter (\p -> snd p == '@') points
          counts = map (\r -> (r, countNeighbors currentMap r)) rolls
          moveable = map fst $ filter ((< 4) . snd) counts
          notMoved :: PointRC -> Char -> Bool
          notMoved k _ = notElem k moveable
          nextMap :: HashMap PointRC Char
          nextMap = HashMap.filterWithKey notMoved currentMap
          movedInRemainder = go nextMap
       in -- trace (show currentMap)
          (if null moveable then 0 else movedInRemainder + length moveable)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i =
  let ls = T.lines i
      cols = map (zip [0 ..] . T.unpack) ls
      rows = zip [0 ..] cols
      cells = [(PointRC r c, val) | (r, cs) <- rows, (c, val) <- cs]
      hashmap = HashMap.fromList cells
   in Right $ GridRC hashmap (length cols) (length rows)

solve :: IO ()
solve = do
  input <- getInput 2025 4
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
