{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day09 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.Hashable (Hashable (..))
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import InputFetcher (getInput)
import Text.Parsec hiding (Line, count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

data Tile = Tile {tx :: Int, ty :: Int} deriving (Eq, Generic, Hashable, Ord)

-- a line defined by two endpoints.
-- we assume that lines are vertical or horizontal, and have been passed through `sortLine`
newtype Line = Line (Tile, Tile)

-- a rectangle defined by two opposite corners
newtype Rect = Rect (Tile, Tile)

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

area :: Rect -> Int
area (Rect (t1, t2)) = (xdist + 1) * (ydist + 1)
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
    allPairs = [Rect (t1, t2) | t1 <- input, t2 <- input]
    sizes = map area allPairs
    result = maximum sizes

tlines :: [Tile] -> [Line]
tlines input = result
  where
    pairs = zipWith (curry Line) input (drop 1 input)
    lastPair = Line (last input, fromJust $ listToMaybe input)
    result = pairs ++ [lastPair]

sortLine :: Line -> Line
sortLine (Line (t1, t2)) = Line (Tile xmin ymin, Tile xmax ymax)
  where
    xmin = min (tx t1) (tx t2)
    xmax = max (tx t1) (tx t2)
    ymin = min (ty t1) (ty t2)
    ymax = max (ty t1) (ty t2)

shrink1 :: Rect -> Rect
shrink1 (Rect (t1, t2)) = Rect (Tile (xmin + 1) (ymin + 1), Tile (xmax - 1) (ymax - 1))
  where
    xmin = min (tx t1) (tx t2)
    xmax = max (tx t1) (tx t2)
    ymin = min (ty t1) (ty t2)
    ymax = max (ty t1) (ty t2)

lineIntersects :: Line -> Line -> Bool
lineIntersects (Line (tmin1, tmax1)) (Line (tmin2, tmax2)) = not separatedInX && not separatedInY
  where
    -- assuming both lines are sorted
    separatedInX = (tx tmin1 >= tx tmax2) || (tx tmin2 >= tx tmax1)
    separatedInY = (ty tmin1 >= ty tmax2) || (ty tmin2 >= ty tmax1)

rectEdges :: Rect -> [Line]
rectEdges rect = rectLines
  where
    Rect (mint, maxt) = rect
    corners = [mint, Tile (tx mint) (ty maxt), maxt, Tile (tx maxt) (ty mint)]
    rectLines = map sortLine $ tlines corners

shrunkRectIsInsideLines :: [Line] -> Rect -> Bool
shrunkRectIsInsideLines ls rect = result
  where
    rectLines = rectEdges $ shrink1 rect
    checks = [(rl, il) | rl <- rectLines, il <- ls]
    result = all (\(rl, il) -> not (lineIntersects rl il)) checks

part2 :: Input -> Int
part2 input = result
  where
    -- redOrGreen = getAllPoints input
    allRects = [Rect (t1, t2) | t1 <- input, t2 <- input]
    ls = map sortLine $ tlines input
    rectsWithSizes = map (\rect -> (rect, area rect)) allRects
    biggestFirst = sortBy (comparing (Data.Ord.Down . snd)) rectsWithSizes
    allPairs' = filter (shrunkRectIsInsideLines ls . fst) biggestFirst
    (_, result) = fromJust $ listToMaybe allPairs'

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
