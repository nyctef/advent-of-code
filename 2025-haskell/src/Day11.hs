module Day11 (solve, part1, part2, parseInput) where

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
import Text.Printf(printf)

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
parseInput i = left show $ parse connectionsP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 11
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
