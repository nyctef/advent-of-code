module Main where

import qualified Day01

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

    putStrLn "Advent of Code 2025"
    putStrLn "==================="
    putStrLn ""

    runDay day

runDay :: Int -> IO ()
runDay 1 = do
    putStrLn "Day 1:"
    Day01.solve
runDay day = do
    putStrLn $ "Day " ++ show day ++ ":"
    putStrLn "  Not yet implemented"
