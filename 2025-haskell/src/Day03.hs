module Day03 (solve, part1, part2, parseInput) where

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
import Data.List
import Data.Maybe
import Text.Parsec.Error (messageString)
import Control.Arrow (left)

newtype Battery = Battery { battPower :: Integer } deriving (Show, Eq, Ord)

newtype Bank = Bank { batteries :: [Battery] } deriving (Show)

newtype Input = Input { banks :: [Bank] } deriving (Show)

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

getMaxJoltage :: Bank -> Integer
getMaxJoltage bank = let
  bs = batteries $ bank
  firstBatt = maximum $ init bs
  firstNum = battPower firstBatt
  firstIndex = elemIndex firstBatt bs
  rest = drop (fromJust firstIndex + 1) bs
  secondNum = battPower $ maximum rest
  result = 10*firstNum + secondNum
  in result

part1 :: Input -> Integer
part1 input =
  let 
      
      

      result = sum $ map getMaxJoltage $ banks input
   in result

part2 :: Input -> Integer
part2 input =
  let result =
        -- trace (show parsed)
        0
   in result

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
