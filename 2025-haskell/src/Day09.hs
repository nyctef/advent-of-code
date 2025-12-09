{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day09 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Control.Exception (assert)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (..))
import Data.List (sortBy)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import InputFetcher (getInput)
import Text.Parsec hiding (count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

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
    sizes = map (uncurry area) allPairs
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

sortLine :: (Tile, Tile) -> (Tile, Tile)
sortLine (t1, t2) = (Tile xmin ymin, Tile xmax ymax)
  where
    xmin = min (tx t1) (tx t2)
    xmax = max (tx t1) (tx t2)
    ymin = min (ty t1) (ty t2)
    ymax = max (ty t1) (ty t2)

pointIsOnLine :: (Tile, Tile) -> Tile -> Bool
pointIsOnLine (mint, maxt) t = result
  where
    inX = tx mint <= tx t && tx t <= tx maxt
    inY = ty mint <= ty t && ty t <= ty maxt
    result = inX && inY

pointIsOnLines :: [(Tile, Tile)] -> Tile -> Bool
pointIsOnLines ls t = any (`pointIsOnLine` t) ls

shrink1 :: (Tile, Tile) -> (Tile, Tile)
shrink1 (t1, t2) = (Tile (xmin + 1) (ymin + 1), Tile (xmax - 1) (ymax - 1))
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
    rectPoints :: [Tile]
    rectPoints = concatMap pointsOnLine rectLines
    result = _traceShow (rect, length rectPoints) all (not . isOnLine) rectPoints

part2 :: Input -> Int
part2 input = result
  where
    -- redOrGreen = getAllPoints input
    allPairs = [(t1, t2) | t1 <- input, t2 <- input]
    ls = map sortLine $ tlines input
    pairsWithSizes = map (\pair -> (pair, uncurry area pair)) allPairs
    biggestFirst = sortBy (comparing (Data.Ord.Down . snd)) pairsWithSizes
    linesFromX = HashMap.fromListWith (++) $ concatMap (\(mint, maxt) -> [(tx mint, [(mint, maxt)]), (tx maxt, [(mint, maxt)])]) ls
    linesFromY = HashMap.fromListWith (++) $ concatMap (\(mint, maxt) -> [(ty mint, [(mint, maxt)]), (ty maxt, [(mint, maxt)])]) ls
    isOnLine p = pointIsOnLines (HashMap.findWithDefault [] (tx p) linesFromX ++ HashMap.findWithDefault [] (ty p) linesFromY) p
    allPairs' = filter (shrunkRectIsInsideLines isOnLine . fst) biggestFirst
    (_, result) = _traceShow allPairs' head allPairs'

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
