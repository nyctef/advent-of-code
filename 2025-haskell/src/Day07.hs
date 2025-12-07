{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day07 (solve, part1, part2, parseInput) where

import Data.Char (isDigit)
import Data.HashMap.Strict (HashMap, empty)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable, hash)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import GHC.Generics (Generic)
import InputFetcher (getInput)
import Text.Printf
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

data PointRC = PointRC {row :: Int, col :: Int} deriving (Eq, Ord, Generic, Hashable)

pleft :: PointRC -> PointRC
pleft p = PointRC (row p) (col p - 1)

pright :: PointRC -> PointRC
pright p = PointRC (row p) (col p + 1)

pup :: PointRC -> PointRC
pup p = PointRC (row p - 1) (col p)

pdown :: PointRC -> PointRC
pdown p = PointRC (row p + 1) (col p)

instance Show PointRC where
  show p = printf "(r%d c%d)" (row p) (col p)

-- instance Hashable PointRC where
-- hashWithSalt s (PointRC r c) = s + (hash r) + (hash c)
data GridRC = GridRC {grid :: HashMap PointRC Char, numCols :: Int, numRows :: Int} deriving (Show)

type Input = GridRC

gpoints :: GridRC -> [PointRC]
gpoints g = [PointRC r c | r <- [0 .. numRows g], c <- [0 .. numCols g]]

gget :: GridRC -> PointRC -> Maybe Char
gget g p = HashMap.lookup p $ grid g

gfind :: GridRC -> Char -> Maybe PointRC
gfind g c = listToMaybe $ fmap fst $ filter ((== c) . snd) $ HashMap.toList $ grid g 

type State1 = ([PointRC], Set PointRC, Set PointRC, Int)
-- state: ([queue of points] [set of seen points] [set of exit points] [split count])
solve1 :: GridRC -> State1 -> State1
-- base case: no more points to search
solve1 g ([], s, e, c) = traceShow ("done", e) ([], s, e, c)
-- optimization: skip a point if it's already seen
solve1 g (( q:qs ), s, e, c) | Set.member q s = traceShow (q, "skipping") (qs, s, e, c)
-- process '.' : just move down
solve1 g (( q:qs ), s, e, c) | (gget g q) == Just '.' || (gget g q) == Just 'S' =
  let n = pdown q
  in traceShow (q, "down", n:qs) (n : qs, Set.insert q s, e, c)
-- process '^' : add beams to sides
solve1 g (( q:qs ), s, e, c) | (gget g q) == Just '^' =
  let n1 = pleft q
      n2 = pright q
  in traceShow ("sides") (n1 : n2 : qs, Set.insert q s, e, c+1)
-- process off the edge: save an exit point
solve1 g ((q:qs), s, e, c) | (gget g q) == Nothing = traceShow (q, "exit") (qs, s, Set.insert q e, c)
-- unhandled case?
solve1 g (qs, s, e, c) = error (show (qs, s, e))

run :: Eq a => (a -> a) -> a -> a
run f x
  | x == f x = x
  | otherwise = run f (f x)

part1 :: Input -> Int
part1 input = result
  where
    start = fromJust $ gfind input 'S'
    step :: State1 -> State1
    step = solve1 input
    init :: State1
    init = ([start], Set.empty, Set.empty, 0)
    state = run step init
    (_, _, _, splitCount) = state
    result = splitCount

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
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
