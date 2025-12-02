module Day02 (solve, part1, part2, isInvalid1, isInvalid2) where

import Data.Char (isSpace)
import Data.Either
import Debug.Trace (trace)
import InputFetcher (getInput)
import Text.Parsec hiding (getInput)
import Text.Parsec.String (Parser)
import GHC.Num (integerLogBase)
import Text.Regex.PCRE

-- TODO: switch to Text to make stuff like this more efficient
strip :: String -> String
strip = reverse . trimLeading . reverse
  where
    trimLeading [] = []
    trimLeading (x : xs) =
      if isSpace x then trimLeading xs else x : xs

data Range = Range
  { lo :: Integer,
    hi :: Integer
  }
  deriving (Show, Eq)

range :: Parser Range
range = do
  n1 <- many1 digit
  _ <- char '-'
  n2 <- many1 digit
  return $ Range (read n1) (read n2)

ranges :: Parser [Range]
ranges = range `sepBy` (char ',')


numDigits :: Integer -> Integer
numDigits i = toInteger $ (integerLogBase 10 i) + 1 

removeWs :: String -> String
removeWs x = filter (not . isSpace) x

isInvalid1 :: Integer -> Bool
isInvalid1 i = let
    d = numDigits i
    divisor = 10 ^ (d `div` 2)
    lowPart = i `mod` divisor
    highPart = i `div` divisor
    result = -- trace (show divisor ++ " " ++ show lowPart ++ " " ++ show highPart)
          (d `mod` 2 == 0 && lowPart == highPart)
  in result

isInvalid2 :: Integer -> Bool
isInvalid2 i = let
    asText = show i
    result = asText =~ "^(\\d+)\\1+$"

  in result



countInRange :: (Integer -> Bool) -> Range -> Integer
countInRange f r = toInteger $ sum $ (filter f) $ [lo r..hi r]


part1 :: String -> Integer
part1 input =
  let text = removeWs $ strip input
      parsed = (parse ranges "" text)
      invalids = (map $ countInRange isInvalid1) <$> parsed
      total = (sum <$> invalids)


      result = -- trace (show parsed)
        (fromRight (-1) total)
   in result

part2 :: String -> Integer
part2 input = 
  let text = removeWs $ strip input
      parsed = (parse ranges "" text)
      invalids = (map $ countInRange isInvalid2) <$> parsed
      total = (sum <$> invalids)


      result = -- trace (show parsed)
        (fromRight (-1) total)
   in result

solve :: IO ()
solve = do
  input <- getInput 2025 2
  putStrLn $ "  Part 1: " ++ show (part1 input)
  putStrLn $ "  Part 2: " ++ show (part2 input)
