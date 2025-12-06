module Main where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  day <- case args of
    [] -> do
      -- No argument provided, use current day of month
      now <- getCurrentTime
      let (_, _, dayOfMonth) = toGregorian (utctDay now)
      return dayOfMonth
    (dayStr : _) -> case readMaybe dayStr of
      Just d | d >= 1 && d <= 12 -> return d
      _ -> do
        TIO.putStrLn "Error: Day must be a number between 1 and 12"
        TIO.putStrLn "Usage: aoc2025 [DAY]"
        return 1 -- Default to day 1 on error
  runDay day

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- Dispatch map for all days
solvers :: Map.Map Int (IO ())
solvers =
  Map.fromList
    [ (1, Day01.solve),
      (2, Day02.solve),
      (3, Day03.solve),
      (4, Day04.solve),
      (5, Day05.solve),
      (6, Day06.solve)
    ]

runDay :: Int -> IO ()
runDay day = do
  TIO.putStrLn $ "Day " <> tshow day <> ":"
  case Map.lookup day solvers of
    Just solver -> solver
    Nothing -> TIO.putStrLn "  Not yet implemented"
