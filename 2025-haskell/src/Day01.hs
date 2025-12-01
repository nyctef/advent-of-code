module Day01 (solve, part1, part2) where

import InputFetcher (getInput)
import Debug.Trace (trace)

part1 :: String -> Int
part1 input = trace ("input :" ++ input) 0

part2 :: String -> Int
part2 input = 0  -- TODO: implement

solve :: IO ()
solve = do
    input <- getInput 2025 1
    putStrLn $ "  Part 1: " ++ show (part1 input)
    putStrLn $ "  Part 2: " ++ show (part2 input)
