{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day09 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..), hash)
import Data.List (inits, find, sort, sortBy)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import InputFetcher (getInput)
import Text.Parsec hiding (count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)
import Data.Ord

data Tile = Tile {tx :: Int, ty :: Int} deriving (Eq)

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


part2 :: Input -> Int
part2 input = result
  where
    result = 0

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
