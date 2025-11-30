module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Day01

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day01Tests]

day01Tests :: TestTree
day01Tests = testGroup "Day 1"
  [ testCase "Part 1 - placeholder" $ do
      Day01.part1 "" @?= 0

  , testCase "Part 2 - placeholder" $ do
      Day01.part2 "" @?= 0
  ]
