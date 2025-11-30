module Day01Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Day01

tests :: TestTree
tests = testGroup "Day 1"
  [ testCase "Part 1 - placeholder" $ do
      Day01.part1 "" @?= 0

  , testCase "Part 2 - placeholder" $ do
      Day01.part2 "" @?= 0
  ]
