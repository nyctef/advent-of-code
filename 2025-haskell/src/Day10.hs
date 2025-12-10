module Day10 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Control.Monad (forM, forM_, replicateM)
-- import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Debug.Trace
import InputFetcher (getInput)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec hiding (Line, count, getInput)
import Text.Parsec.Text (Parser)
import Z3.Monad

data Machine = Machine {mLights :: [Bool], mLightTargets :: [Bool], mButtonWirings :: [[Int]], mJoltages :: [Int]} deriving (Show)

pressButton :: Int -> Machine -> Machine
pressButton b m = Machine newLights (mLightTargets m) (mButtonWirings m) (mJoltages m)
  where
    wiring = mButtonWirings m !! b
    oldLights = mLights m
    newLights = zipWith (curry (\(i, l) -> if i `elem` wiring then not l else l)) [0 ..] oldLights

pressButtons :: [Bool] -> Machine -> Machine
pressButtons bs m = newMachine
  where
    indexes = [0 ..] `zip` bs
    lights = mapMaybe (\(i, b) -> if b then Just i else Nothing) indexes
    newMachine = foldl' (flip pressButton) m lights

allCombinations :: Int -> [[Bool]]
allCombinations n = replicateM n [True, False]

isSolved :: Machine -> Bool
isSolved m = lights == targets
  where
    lights = mLights m
    targets = mLightTargets m

countSteps1a :: Machine -> Int
countSteps1a m = _traceShow result result
  where
    candidates = allCombinations $ length $ mButtonWirings m
    attempts = map (\c -> (length $ filter id c, pressButtons c m)) candidates
    solved :: [(Int, Machine)]
    solved = filter (\(_, x) -> isSolved x) attempts
    best = sortBy (comparing fst) solved
    result = fst . fromJust $ listToMaybe $ best

solveIntegerLP :: Int -> [([Int], Int)] -> Integer
solveIntegerLP numVars coefficients = unsafePerformIO $ evalZ3 $ do
  vars <- mapM (\i -> mkFreshIntVar ("x" ++ show i)) [0 .. numVars - 1]
  -- assert all vars > 0
  zero <- mkInteger 0
  mapM_ (\v -> optimizeAssert =<< mkGe v zero) vars

  -- liftIO $ putStrLn "constraints"
  forM_ coefficients $ \(coeffs, rhs) -> do
    terms <- forM (zip coeffs [0 ..]) $ \(c, i) -> do
      let var = vars !! i
      coeff <- mkInteger (toInteger c)
      mkMul [coeff, var]

    expr <- mkAdd terms

    rhsvar <- mkInteger (toInteger rhs)

    constraint <- mkEq expr rhsvar
    -- constraintStr <- astToString constraint
    -- liftIO $ putStrLn $ constraintStr
    optimizeAssert constraint

  -- shortcut: just assume we're minimizing sum(vars)
  -- if copy/pasting this code later, this would need to be parameterized
  objective <- mkAdd vars

  -- objStr <- astToString objective
  -- liftIO $ putStrLn "objective"
  -- liftIO $ putStrLn objStr
  -- TODO: what is the value returned from this function?
  _ <- optimizeMinimize objective

  -- liftIO $ putStrLn "assertions"
  -- assertions <- optimizeGetAssertions
  -- forM_ assertions $ \ast -> do
  -- s <- astToString ast
  -- liftIO $ putStrLn $ "  " ++ s

  result <- optimizeCheck []
  case result of
    Sat -> do
      model <- optimizeGetModel
      -- modelstr <- modelToString model
      -- liftIO $ putStrLn "model"
      -- liftIO $ putStrLn modelstr

      objVal <- evalInt model objective
      return $ fromJust objVal
    _ -> error "unsat"

countSteps2 :: Machine -> Int
countSteps2 m = _traceShow result result
  where
    constraints =
      [ ([if fst j `elem` bw then 1 else 0 | bw <- mButtonWirings m], snd j)
      | j <- zip [0 ..] (mJoltages m)
      ]
    result = fromInteger $ solveIntegerLP (length $ mButtonWirings m) constraints

intP :: Parser Int
intP = read <$> many1 digit

onP :: Parser Bool
onP = True <$ char '#'

offP :: Parser Bool
offP = False <$ char '.'

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
  Machine (replicate (length targets) False) targets wirings <$> joltagesP

machinesP :: Parser [Machine]
machinesP = machineP `sepBy` char '\n'

type Input = [Machine]

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

part1 :: Input -> Int
part1 input = result
  where
    stepCounts = map countSteps1a input
    result = _traceShow stepCounts (sum stepCounts)

part2 :: Input -> Int
part2 input = result
  where
    stepCounts = map countSteps2 input
    result = sum stepCounts

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
