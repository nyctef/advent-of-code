{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day10 (solve, part1, part2, parseInput) where

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

data Machine = Machine { mButtons :: [Bool], mButtonTargets :: [Bool], mButtonWirings :: [[Int]], mJoltages :: [Int] } deriving (Show)

intP :: Parser Int
intP = read <$> many1 digit

onP :: Parser Bool
onP = return True <$> char '#'

offP :: Parser Bool
offP = return False <$> char '.'

buttonP :: Parser Bool
buttonP = choice [onP, offP]

buttonTargetsP :: Parser [Bool]
buttonTargetsP = do
  _ <- char '['
  targets <- many1 buttonP
  _ <- char ']'
  return targets

buttonWiringP :: Parser [Int]
buttonWiringP = do
  _ <- char '('
  wiring <- intP `sepBy` char ','
  _ <- char ')'
  _ <- char ' '
  return wiring

buttonWiringsP :: Parser [[Int]]
-- we want to use sepBy ' ' here, but then it's ambiguous whether
-- to parse another button wiring or start joltages so we instead
-- stash the trailing space into buttonWiringP and use the ( vs {
-- character to disambiguate
buttonWiringsP = many1 buttonWiringP

joltagesP :: Parser [Int]
joltagesP = do
  _ <- char '{'
  wiring <- intP `sepBy` char ','
  _ <- char '}'
  return wiring

machineP :: Parser Machine
machineP = do
  targets <- buttonTargetsP
  _ <- char ' '
  wirings <- buttonWiringsP
  joltages <- joltagesP
  return $ Machine (replicate (length targets) False) targets wirings joltages

machinesP :: Parser [Machine]
machinesP = machineP `sepBy` char '\n'


type Input = [Machine]

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

part1 :: Input -> Int
part1 input = result
  where
    result = 0

part2 :: Input -> Int
part2 input = result
  where
    result = 0

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = left show $ parse machinesP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 10
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
