


module Day10 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Hashable (Hashable (..))
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import GHC.Float
import GHC.Generics
import InputFetcher (getInput)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec hiding (Line, count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)
import Z3.Monad

data Machine = Machine {mLights :: [Bool], mLightTargets :: [Bool], mButtonWirings :: [[Int]], mJoltages :: [Int]} deriving (Show)

numButtons :: Machine -> Int
numButtons = length . mButtonWirings

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

-- apparently this isn't built in? https://stackoverflow.com/questions/5852722
-- replaceNth :: Int -> (a -> a) -> [a] -> [a]
-- replaceNth _ _ [] = []
-- replaceNth n mutate (x:xs)
-- \| n == 0 = (mutate x) : xs
-- \| otherwise = x : replaceNth (n-1) mutate xs

allCombinations :: Int -> [[Bool]]
allCombinations n = replicateM n [True, False]

-- machine, step count, already-seen light configurations
type State1 = (Machine, Int, Set [Bool])

isSolved :: Machine -> Bool
isSolved m = lights == targets
  where
    lights = mLights m
    targets = mLightTargets m

foundSolution :: Seq State1 -> Bool
foundSolution (x :<| xs) = isSolved m
  where
    (m, _, _) = x

-- because laziness
seqHead :: Seq a -> a
seqHead (x :<| xs) = x
seqHead _ = undefined

countSteps1 :: Machine -> Int
countSteps1 m = _traceShow result result
  where
    seed = Seq.singleton (m, 0, Set.empty)
    step :: Seq State1 -> Seq State1
    step (x :<| xs) =
      let (m', c, s) = x
          nextMachines = filter (not . (`Set.member` s) . mLights) $ map (`pressButton` m') [0 .. length (mButtonWirings m') - 1]
          nextSeen = Set.insert (mLights m') s
          nexts = Seq.fromList $ map (\m -> (m, c + 1, nextSeen)) nextMachines
       in _traceShow (length xs, length nextSeen) (if Set.member (mLights m') s then xs else xs Seq.>< nexts)
    steps = iterate step seed
    final = dropWhile (not . foundSolution) steps
    result = (\(_, x, _) -> x) $ seqHead $ head final

countSteps1a :: Machine -> Int
countSteps1a m = _traceShow result result
  where
    candidates = allCombinations $ length $ mButtonWirings m
    attempts = map (\c -> (length $ filter id c, pressButtons c m)) candidates
    solved :: [(Int, Machine)]
    solved = filter (\(_, m) -> isSolved m) attempts
    best = sortBy (comparing fst) solved
    result = fst . head $ best

getMaxButtonPresses :: Machine -> [Int]
getMaxButtonPresses m = result
  where
    -- for each button wiring: find the minimum joltage that it corresponds to
    -- since we can't press a button more often than that
    ws = mButtonWirings m
    js = mJoltages m
    result = map (minimum . map (\x -> js !! x)) ws

candidates2 :: Machine -> [[Int]]
candidates2 m = mapM (\x -> [0 .. x]) (getMaxButtonPresses m)

pressN :: (Int, [Int]) -> [Int] -> [Int]
pressN (c, wirings) prev = result
  where
    indexed = [0 ..] `zip` prev
    result = map (\(i, p) -> if i `elem` wirings then p + c else p) indexed

isSolved2 :: Machine -> [Int] -> Bool
isSolved2 m presses = result
  where
    target = mJoltages m
    presses' = presses `zip` mButtonWirings m
    totals = foldr pressN (replicate (length target) 0) presses'
    result = totals == target

-- countSteps2 m = traceShow result result
--   where
--     minimize = map int2Double $ replicate (length $ mButtonWirings m) 1
--     constraints = [[if (fst j) `elem` bw then 1.0 else 0.0 | bw <- mButtonWirings m ] :==: (int2Double (snd j))|
--       j <- zip [0..] (mJoltages m) ]
--     bounds = []
--     solution = simplex (Minimize minimize) (Dense constraints) bounds
--     result = traceShow (minimize, constraints) traceShow (solution) (readSolution solution)
--
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

  objective <- mkAdd vars

  -- objStr <- astToString objective
  -- liftIO $ putStrLn "objective"
  -- liftIO $ putStrLn objStr
  optimizeMinimize objective

  -- liftIO $ putStrLn "assertions"
  assertions <- optimizeGetAssertions
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
