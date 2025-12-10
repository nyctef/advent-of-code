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
import Debug.Trace
import qualified Data.Set as Set
import Data.Set(Set(..))
import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))

data Machine = Machine { mLights :: [Bool], mLightTargets :: [Bool], mButtonWirings :: [[Int]], mJoltages :: [Int] } deriving (Show)

numButtons :: Machine -> Int
numButtons = length . mButtonWirings

pressButton :: Int -> Machine -> Machine
pressButton b m = Machine newLights (mLightTargets m) (mButtonWirings m) (mJoltages m)
  where
    wiring = (mButtonWirings m) !! b
    oldLights = (mLights m)
    newLights = map (\(i, l) -> if i `elem` wiring then not l else l) $ zip [0..] oldLights


-- apparently this isn't built in? https://stackoverflow.com/questions/5852722
-- replaceNth :: Int -> (a -> a) -> [a] -> [a]
-- replaceNth _ _ [] = []
-- replaceNth n mutate (x:xs)
  -- | n == 0 = (mutate x) : xs
  -- | otherwise = x : replaceNth (n-1) mutate xs


-- machine, step count, already-seen light configurations
type State1 = (Machine, Int, Set [Bool])

foundSolution :: Seq State1 -> Bool
foundSolution (x:<| xs) = lights == targets
  where
    (m, _, _) = x
    lights = mLights m
    targets = mLightTargets m

-- because laziness
seqHead :: Seq a -> a
seqHead (x :<| xs) = x
seqHead _ = undefined

countSteps1 :: Machine -> Int
countSteps1 m = traceShow result result
  where
    seed = Seq.singleton (m, 0, Set.empty)
    step :: Seq State1 -> Seq State1
    step (x :<| xs) = 
      let
        (m', c, s) = x
        nextMachines = filter (not . (`Set.member` s) . mLights) $ map (\b -> pressButton b m') [0..(length $ mButtonWirings m') - 1]
        nextSeen = Set.insert (mLights m') s
        nexts = Seq.fromList $ map (\m -> (m, c+1, nextSeen)) nextMachines
      in _traceShow (length xs, length nextSeen) (if Set.member (mLights m') s then xs else xs Seq.>< nexts)
    steps = iterate step seed
    final = dropWhile (not . foundSolution) steps
    result = (\(_,x,_) -> x) $ seqHead $ head final

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
    stepCounts = map countSteps1 input
    result = _traceShow stepCounts (sum stepCounts)

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
