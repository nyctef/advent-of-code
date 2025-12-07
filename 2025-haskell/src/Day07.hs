module Day07 (solve, part1, part2, parseInput) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import InputFetcher (getInput)
import qualified PointRC as P
import PointRC(PointRC(..))


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

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

type State1 = ([PointRC], Set PointRC, Set PointRC, Int)

-- state: ([queue of points] [set of seen points] [set of exit points] [split count])
solve1 :: GridRC -> State1 -> State1
-- base case: no more points to search
solve1 _ ([], s, e, c) = _traceShow ("done" :: String, e) ([], s, e, c)
solve1 g (q : qs, s, e, c)
  -- optimization: skip a point if it's already seen
  | Set.member q s = _traceShow (q, "skipping" :: String) (qs, s, e, c)
  -- process '.' : just move down
  | gget g q == Just '.' || gget g q == Just 'S' =
      let n = P.down q
       in _traceShow (q, "down" :: String, n : qs) (n : qs, Set.insert q s, e, c)
  -- process '^' : add beams to sides
  | gget g q == Just '^' =
      let n1 = P.left q
          n2 = P.right q
       in _traceShow ("sides" :: String) (n1 : n2 : qs, Set.insert q s, e, c + 1)
  -- process off the edge: save an exit point
  | isNothing (gget g q) = _traceShow (q, "exit" :: String) (qs, s, Set.insert q e, c)
  -- unhandled case?
  | otherwise = error (show (qs, s, e))

run :: (Eq a) => (a -> a) -> a -> a
run f x
  | x == f x = x
  | otherwise = run f (f x)

part1 :: Input -> Int
part1 input = result
  where
    start = fromJust $ gfind input 'S'
    step :: State1 -> State1
    step = solve1 input
    seed :: State1
    seed = ([start], Set.empty, Set.empty, 0)
    state = run step seed
    (_, _, _, splitCount) = state
    result = splitCount

type State2 = HashMap PointRC Int
solve2 :: GridRC -> State2 -> PointRC -> State2
solve2 g s p =
    _traceShow (p, fromleft, fromup, fromright) HashMap.insert p (fromleft + fromup + fromright) s
  where
    upleftP = (P.up . P.left) p
    upleftC = gget g upleftP
    upP = P.up p
    upC = gget g upP
    uprightP = (P.up . P.right) p
    uprightC = gget g uprightP
    fromleft = if upleftC == Just '^' then HashMap.findWithDefault 0 upleftP s else 0
    fromup = if upC == Just '.' || upC == Just 'S' then HashMap.findWithDefault 0 upP s else 0
    fromright = if uprightC == Just '^' then HashMap.findWithDefault 0 uprightP s else 0

part2 :: Input -> Int
part2 input = result
  where
    start = fromJust $ gfind input 'S'
    seed :: HashMap PointRC Int
    seed = HashMap.fromList [(start, 1)]
    points = filter (\p -> P.row p /= 0) (gpoints input)
    state = foldl (solve2 input) seed points
    bottomRow = map (PointRC (numRows input - 1)) [0..numCols input - 1]
    total = sum $ map (\p -> HashMap.findWithDefault 0 p state) bottomRow

    result = _traceShow state total

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i =
  let ls = T.lines i
      cols = map (zip [0 ..] . T.unpack) ls
      rows = zip [0 ..] cols
      cells = [(PointRC r c, val) | (r, cs) <- rows, (c, val) <- cs]
      hashmap = HashMap.fromList cells
   in Right $ GridRC hashmap (length $ fromJust $ listToMaybe cols) (length cols)

solve :: IO ()
solve = do
  input <- getInput 2025 7
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
