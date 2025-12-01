{-# LANGUAGE MultilineStrings #-}

module Day01Test where

import qualified Day01
import Test.Tasty
import Test.Tasty.HUnit

test_part1 :: TestTree
test_part1 = testCase "Day 1 - Part 1" $ do
  Day01.part1
    """
    L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
    """
    @?= 3

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 1 - Part 2"
    [ testCase "example1" $ do
        Day01.part2 "" @?= 0
    ]
