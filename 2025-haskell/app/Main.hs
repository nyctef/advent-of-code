module Main where

import qualified Day01

import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
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
        (dayStr:_) -> case readMaybe dayStr of
            Just d | d >= 1 && d <= 12 -> return d
            _ -> do
                putStrLn "Error: Day must be a number between 1 and 12"
                putStrLn "Usage: aoc2025 [DAY]"
                return 1  -- Default to day 1 on error

    runDay day

-- Dispatch map for all days
solvers :: Map.Map Int (IO ())
solvers = Map.fromList
  [ (1, Day01.solve)
  -- Add more days here as you implement them:
  -- , (2, Day02.solve)
  -- , (3, Day03.solve)
  ]

runDay :: Int -> IO ()
runDay day = do
    putStrLn $ "Day " ++ show day ++ ":"
    case Map.lookup day solvers of
        Just solver -> solver
        Nothing -> putStrLn "  Not yet implemented"
