{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Day08 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
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
import Text.Parsec hiding (count, getInput)
import Text.Parsec.Text (Parser)
import Debug.Trace
import Data.Hashable (Hashable(..), hash)
import GHC.Generics
import Data.List (sort, inits, scanl')
import Text.Printf (printf)

data Box = Box {bx :: Int, by :: Int, bz :: Int} deriving (Eq, Generic, Hashable)

instance Show Box where
  show b = printf "[%d,%d,%d]" (bx b) (by b) (bz b)

boxP :: Parser Box
boxP = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  _ <- char ','
  z <- many1 digit
  return $ Box (read x) (read y) (read z)

boxesP :: Parser [Box]
boxesP = boxP `sepBy` char '\n'

data Connection = Connection {cdist :: Int, b1 :: Box, b2 :: Box} deriving (Show)

instance Eq Connection where
  a == b = (b1 a == b1 b) && (b2 a == b2 b) || (b1 a == b2 b) && (b2 a == b1 b)

instance Hashable Connection where
  hashWithSalt s a = (hash $ cdist a) + (hash $ b1 a) + (hash $ b2 a)

instance Ord Connection where
  compare a b = compare (cdist a) (cdist b)

type Input = [Box]

connect :: Box -> Box -> Connection
-- TODO: are all boxes integer distances apart?
connect b1 b2 = Connection (round dist) b1 b2
  where
    dist :: Double
    dist = (sqrt ) (fromIntegral ( dx * dx + dy * dy + dz * dz ))
    dx = bx b2 - bx b1
    dy = by b2 - by b1
    dz = bz b2 - bz b1

allConnections :: [Box] -> [Connection]
allConnections bs = concatMap (\b1 -> map (connect b1) $ filter (\b2 -> b2 /= b1) bs) bs

-- (unused connections, current set, completed circuit sizes)
type State1 = (HashSet Connection, HashSet Box, [Int])

canConnect :: HashSet Connection -> HashSet Box -> Maybe Connection
canConnect cs bs = listToMaybe $ filter (\c -> HashSet.member (b1 c) bs || HashSet.member (b2 c) bs) $ HashSet.toList cs

getTrees :: State1 -> State1
getTrees (p, c, d)
  -- done
  | null p && null c = (p, c, d)
  -- no current set, so start one
  | null c = 
      let
         next = head $ HashSet.toList p
         p' = HashSet.delete next p
         c' = HashSet.insert (b1 next) $ HashSet.insert (b2 next) c
      in (p', c', d)
  -- current set and a connection can be added
  | Just next <- canConnect p c =
      let
         p' = HashSet.delete next p
         c' = HashSet.insert (b1 next) $ HashSet.insert (b2 next) c
      in (p', c', d)
  -- end current set and record its size
  | otherwise =
      let
        d' = (length c) : d
        c' = HashSet.empty
      in (p, c', d')


-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

run :: (Eq a) => (a -> a) -> a -> a
run f x
  | x == f x = x
  | otherwise = run f (f x)

part1 :: Int -> Input -> Int
part1 count input = result
  where
    conns = take count $ sort $ HashSet.toList $ HashSet.fromList $ allConnections input
    seed = (HashSet.fromList conns, HashSet.empty, [])
    (_, _, sizes) = run getTrees seed
    top3 = take 3 $ reverse $ sort sizes
    result = (product top3)

getTreeSizes :: [Connection] -> [Int]
getTreeSizes cs = let
    seed = (HashSet.fromList cs, HashSet.empty, [])
    (_, _, sizes) = run getTrees seed
  in sizes
  

part2 :: Input -> Int
part2 input = result
  where
    attempts = inits $ sort $ HashSet.toList $ HashSet.fromList $ allConnections input
    sizes = map (\a -> (a, getTreeSizes a)) attempts
    complete = filter (\(_, s) -> (length s == 1) && (listToMaybe s == Just (length input))) sizes
    (aconns, _)  = (head complete)
    aconn = last aconns
    result = _traceShow ( aconns) ((bx $ b1 aconn) * (bx $ b2 aconn))

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = left show $ parse boxesP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 8
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow ((part1 1000) <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
