module Day11 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import InputFetcher (getInput)
import Text.Parsec hiding (Line, count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf(printf)
import Data.HashMap.Strict(HashMap(..), (!))
import qualified Data.HashMap.Strict as HashMap

data Connection = Connection { getFrom :: Text , getTo :: [Text] }
instance Show Connection where
  show (Connection from to) = printf "%s -> %s" from ("," `T.intercalate` to)

type Input = [Connection]

connectionP :: Parser Connection
connectionP = do
  from <- many1 alphaNum
  _ <- char ':'
  _ <- char ' '
  to <- (many1 alphaNum) `sepBy` char ' '
  let to' = map T.pack to
  return $ Connection (T.pack from) to'

connectionsP :: Parser [Connection]
connectionsP = connectionP `sepBy` char '\n'

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

toMap :: [Connection] -> HashMap Text [Text]
toMap cs = HashMap.fromList $ map (\c -> (getFrom c, getTo c)) cs

countPaths :: HashMap Text [Text] -> Text -> Text -> Int
countPaths map start end
  | start == end = 1
  | otherwise = sum $ fmap (\n -> countPaths map n end) (map ! start)

    

part1 :: Input -> Int
part1 input = result
  where
    map = toMap input
    start = map ! "you"
    result = countPaths map "you" "out"

part2 :: Input -> Int
part2 input = result
  where
    result = 0

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = left show $ parse connectionsP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 11
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
