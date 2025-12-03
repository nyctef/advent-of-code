module Day03 (solve, part1, part2, parseInput) where

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

newtype Battery = Battery {battPower :: Integer} deriving (Eq, Ord)

instance Show Battery where
  show b = "(" ++ (show (battPower b)) ++ ")"

newtype Bank = Bank {batteries :: [Battery]} deriving (Show)

newtype Input = Input {banks :: [Bank]} deriving (Show)

batteryP :: Parser Battery
-- battery = read <$> digit
batteryP = do
  d <- digit
  let n = read [d]
  return $ Battery n

bankP :: Parser Bank
bankP = Bank <$> many1 batteryP

inputP :: Parser Input
inputP = Input <$> bankP `sepBy` (char '\n')

getMaxJoltage1 :: Bank -> Integer
getMaxJoltage1 bank =
  let bs = batteries $ bank
      firstBatt = maximum $ init bs
      firstNum = battPower firstBatt
      firstIndex = elemIndex firstBatt bs
      rest = drop (fromJust firstIndex + 1) bs
      secondNum = battPower $ maximum rest
      result = 10 * firstNum + secondNum
   in result

initN :: Int -> [a] -> [a]
initN n xs = take (length xs - n) xs

getMaxJoltage2 :: Integer -> Bank -> Integer
getMaxJoltage2 count bank =
  let bs = batteries $ bank
      firstBatt = maximum $ initN (fromIntegral (count - 1)) bs
      firstNum = battPower firstBatt
      firstIndex = elemIndex firstBatt bs
      rest = drop (fromJust firstIndex + 1) bs
      secondNum = getMaxJoltage2 (count - 1) (Bank rest)
      concatted = read (show firstNum ++ show secondNum)
      result = if count == 1 then firstNum else concatted
   in trace (printf "bank %s count %d -> firstNum %d result %d" (show bank) count firstNum result) result

part1 :: Input -> Integer
part1 input = sum $ map getMaxJoltage1 $ banks input

part2 :: Input -> Integer
part2 input = sum $ map (getMaxJoltage2 12) $ banks input

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = left show $ parse inputP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 3
  let parsed = parseInput input
  TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
