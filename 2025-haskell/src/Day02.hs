module Day02 (solve, part1, part2, isInvalid1, isInvalid2) where

import Data.Char (isSpace)
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Num (integerLogBase)
import InputFetcher (getInput)
import Text.Parsec hiding (getInput)
import Text.Parsec.Text (Parser)
import Text.Regex.PCRE


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
ranges = range `sepBy` char ','

numDigits :: Integer -> Integer
numDigits i = toInteger $ integerLogBase 10 i + 1

isInvalid1 :: Integer -> Bool
isInvalid1 i =
  let d = numDigits i
      divisor = 10 ^ (d `div` 2)
      lowPart = i `mod` divisor
      highPart = i `div` divisor
      result =
        -- trace (show divisor ++ " " ++ show lowPart ++ " " ++ show highPart)
        (even d && lowPart == highPart)
   in result

isInvalid2 :: Integer -> Bool
isInvalid2 i = show i =~ ("^(\\d+)\\1+$" :: String)

countInRange :: (Integer -> Bool) -> Range -> Integer
countInRange f r = toInteger $ sum $ filter f [lo r .. hi r]

part1 :: Text -> Integer
part1 input =
  let text = T.filter (not . isSpace) input
      parsed = parse ranges "" text
      invalids = map (countInRange isInvalid1) <$> parsed
      total = sum <$> invalids

      result =
        -- trace (show parsed)
        fromRight (-1) total
   in result

part2 :: Text -> Integer
part2 input =
  let text = T.filter (not . isSpace) input
      parsed = parse ranges "" text
      invalids = map (countInRange isInvalid2) <$> parsed
      total = sum <$> invalids

      result =
        -- trace (show parsed)
        fromRight (-1) total
   in result

tshow :: Show a => a -> Text
tshow = T.pack . show

solve :: IO ()
solve = do
  input <- getInput 2025 2
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 input)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 input)
