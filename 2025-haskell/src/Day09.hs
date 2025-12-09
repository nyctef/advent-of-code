{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day09 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..), hash)
import Data.List (find, group, inits, sort, sortBy)
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import GHC.Generics
import InputFetcher (getInput)
import Text.Parsec hiding (count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)
import Control.Exception (assert)

data Tile = Tile {tx :: Int, ty :: Int} deriving (Eq, Generic, Hashable, Ord)

instance Show Tile where
  show t = printf "[%d,%d]" (tx t) (ty t)

tileP :: Parser Tile
tileP = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  return $ Tile (read x) (read y)

tilesP :: Parser [Tile]
tilesP = tileP `sepBy` char '\n'

type Input = [Tile]

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

run :: (Eq a) => (a -> a) -> a -> a
run f x
  | x == f x = x
  | otherwise = run f (f x)

area :: Tile -> Tile -> Int
area t1 t2 = (xdist + 1) * (ydist + 1)
  where
    xmin = min (tx t1) (tx t2)
    xmax = max (tx t1) (tx t2)
    ymin = min (ty t1) (ty t2)
    ymax = max (ty t1) (ty t2)
    xdist = xmax - xmin
    ydist = ymax - ymin

part1 :: Input -> Int
part1 input = result
  where
    allPairs = [(t1, t2) | t1 <- input, t2 <- input]
    sizes = map (\(t1, t2) -> area t1 t2) allPairs
    result = maximum sizes

tlines :: [Tile] -> [(Tile, Tile)]
tlines input = result
  where
    pairs = zip input $ tail input
    lastPair = (last input, head input)
    result = pairs ++ [lastPair]

pointsOnLine :: (Tile, Tile) -> [Tile]
pointsOnLine (t1, t2) = assert (tx t1 == tx t2 || ty t1 == ty t2) result
  where
    xmin = min (tx t1) (tx t2)
    xmax = max (tx t1) (tx t2)
    ymin = min (ty t1) (ty t2)
    ymax = max (ty t1) (ty t2)
    result = [Tile x y | x <- [xmin .. xmax], y <- [ymin .. ymax]]

dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort

flipInOut :: HashSet Tile -> (Bool, [Tile]) -> Tile -> (Bool, [Tile])
flipInOut boundaries (inside, collected) next
  | HashSet.member next boundaries = (not inside, next : collected)
  | inside = (inside, next : collected)
  | otherwise = (inside, collected)

traceLine :: HashSet Tile -> [Tile] -> HashSet Tile
traceLine boundaries row = result
  where
    folder :: (Bool, [Tile]) -> Tile -> (Bool, [Tile])
    folder = flipInOut boundaries
    seed :: (Bool, [Tile])
    seed = (False, [])
    state = foldl' folder seed row
    result = HashSet.fromList $ snd state

-- getFilledPoints :: HashSet Tile -> HashSet Tile
-- getFilledPoints linePoints = result
--   where
--     linePoints' = HashSet.toList linePoints
--     xmin = ((-) 1) $ minimum $ map tx linePoints'
--     ymin = ((-) 1) $ minimum $ map ty linePoints'
--     xmax = ((+) 1) $ maximum $ map tx linePoints'
--     ymax = ((+) 1) $ maximum $ map ty linePoints'
--     candidates :: [[Tile]]
--     candidates = [[Tile x y | y <- [ymin .. ymax]] | x <- [xmin .. xmax]]
--     insides = map (traceLine linePoints) candidates
--     result = HashSet.unions insides

-- getAllPoints :: [Tile] -> HashSet Tile
-- getAllPoints input = result
--   where
--     seed = HashSet.empty
--     lines = tlines input
--     pointsOnLines = HashSet.fromList $ concatMap pointsOnLine lines
--     result = getFilledPoints pointsOnLines

-- allPointsIn :: HashSet Tile -> (Tile, Tile) -> Bool
-- allPointsIn redOrGreen (t1, t2) = all (`HashSet.member` redOrGreen) filled
--   where
--     xmin = min (tx t1) (tx t2)
--     xmax = max (tx t1) (tx t2)
--     ymin = min (ty t1) (ty t2)
--     ymax = max (ty t1) (ty t2)
--     xdist = xmax - xmin
--     ydist = ymax - ymin
--     filled = [Tile x y | x <- [xmin .. xmax], y <- [ymin .. ymax]]

pointIsInsideLines :: HashSet Tile -> Tile -> Bool
pointIsInsideLines lines t
  | t `elem` lines = False -- hoping we don't need to deal with this edge case if we shrink the rect first
  | otherwise = let
        path = [Tile x (ty t) | x <- [tx t..100000]]
        count = foldl' (\c n -> if (n `HashSet.member` lines) then c + 1 else c) 0 path
      in odd count

shrink1 :: (Tile, Tile) -> (Tile, Tile)
shrink1 (t1, t2) = (Tile (xmin + 1) (ymin + 1), Tile (xmax -1)( ymax - 1))
  where
    xmin = min (tx t1) (tx t2)
    xmax = max (tx t1) (tx t2)
    ymin = min (ty t1) (ty t2)
    ymax = max (ty t1) (ty t2)

shrunkRectIsInsideLines :: (Tile -> Bool) -> (Tile, Tile) -> Bool
shrunkRectIsInsideLines isOnLine rect = result
  where
    (mint, maxt) = shrink1 rect
    corners = [mint, Tile (tx mint) (ty maxt), maxt, Tile (tx maxt) (ty mint)]
    rectLines = tlines corners
    rectPoints  :: [Tile]
    rectPoints = concatMap pointsOnLine rectLines
    result = traceShow (length rectPoints) all (not . isOnLine) rectPoints

part2 :: Input -> Int
part2 input = result
  where
    -- redOrGreen = getAllPoints input
    allPairs = [(t1, t2) | t1 <- input, t2 <- input]
    lines = tlines input
    pointsOnLines = HashSet.fromList $ concatMap pointsOnLine lines
    possibleLineXs = HashSet.fromList $ map tx input
    possibleLineYs = HashSet.fromList $ map ty input
    isOnLine p = (HashSet.member (tx p) possibleLineXs) && (HashSet.member (ty p) possibleLineYs) && (HashSet.member p pointsOnLines)
    allPairs' = traceShow (length allPairs, length pointsOnLines) filter (shrunkRectIsInsideLines isOnLine) allPairs
    sizes = map (\(t1, t2) -> area t1 t2) allPairs'
    result = maximum sizes
    -- result = traceShow sizes (-1)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = left show $ parse tilesP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 9
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
