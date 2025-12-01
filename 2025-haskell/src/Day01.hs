module Day01 (solve, part1, part2) where

import Data.Char (isSpace)
import Data.Either
import Debug.Trace (trace)
import InputFetcher (getInput)
import Text.Parsec hiding (getInput)
import Text.Parsec.String (Parser)

-- TODO: switch to Text to make stuff like this more efficient
strip :: String -> String
strip = reverse . trimLeading . reverse
  where
    trimLeading [] = []
    trimLeading (x : xs) =
      if isSpace x then trimLeading xs else x : xs

data Dir = L | R deriving (Show, Eq)

data Instruction = Instruction
  { dir :: Dir,
    num :: Int
  }
  deriving (Show, Eq)

instruction :: Parser Instruction
instruction = do
  d <- char 'L' <|> char 'R'
  n <- many1 digit
  return $ Instruction (if d == 'L' then L else R) (read n)

instructions :: Parser [Instruction]
instructions = instruction `sepBy` (string "\n")

toInt :: Instruction -> Int
toInt (Instruction L n) = -n
toInt (Instruction R n) = n

part1 :: String -> Int
part1 input =
  let text = strip input
      parsed = (parse instructions "" text)
      diffs = (map toInt) <$> parsed
      rotate x y = (x + y) `mod` 100
      steps = scanl rotate (50 :: Int) <$> diffs
      numZeros = length <$> filter (== 0) <$> steps
      result = fromRight (-1) numZeros
   in result

part2 :: String -> Int
part2 input =
  let text = strip input
      parsed = (parse instructions "" text)
      diffs = (map toInt) <$> parsed
      rotate :: (Int, Int) -> Int -> (Int, Int)
      rotate (x, _) y =
        let hack = if x == 0 && (x + y) < 0 then -1 else 0
            newPos = (x + y) `mod` 100
            zeros = if (x + y) == 0 then 1 else (abs $ (x + y) `div` 100)
            res = zeros + hack
         in (newPos, res)
      steps = scanl rotate (50 :: Int, 0) <$> diffs
      numZeros = sum . (map snd) <$> steps

      result = trace (show steps) (fromRight (-1) numZeros)
   in result

solve :: IO ()
solve = do
  input <- getInput 2025 1
  putStrLn $ "  Part 1: " ++ show (part1 input)
  putStrLn $ "  Part 2: " ++ show (part2 input)
