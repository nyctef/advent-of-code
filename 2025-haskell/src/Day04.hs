{-# LANGUAGE DeriveGeneric #-}

module Day04 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import InputFetcher (getInput)
import Text.Parsec hiding (count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable, hash)
import GHC.Generics (Generic)

data PointRC = PointRC { row :: Integer, col :: Integer } deriving (Eq, Generic)
instance Show PointRC where
  show p = printf "(r%d c%d)" (row p) (col p)
-- instance Hashable PointRC where
-- hashWithSalt s (PointRC r c) = s + (hash r) + (hash c)

data Input = Input { grid :: HashMap PointRC Char } deriving (Show)


part1 :: Input -> Integer
part1 input = undefined

part2 :: Input -> Integer
part2 input = undefined

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = undefined

solve :: IO ()
solve = do
  input <- getInput 2025 3
  let parsed = parseInput input
  TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
