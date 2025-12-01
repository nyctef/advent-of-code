{-# LANGUAGE MultilineStrings #-}

module Day01Test where

import qualified Day01
import Test.Tasty
import Test.Tasty.HUnit

example1 =
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

test_part1 :: TestTree
test_part1 = testCase "Day 1 - Part 1" $ do
  Day01.part1
    example1
    @?= 3

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 1 - Part 2"
    [ testCase "example1" $
        Day01.part2 example1 @?= 6,
      testCase "example2" $
        Day01.part2 "R1000" @?= 10,
      testCase "example2b" $
        Day01.part2 "L1000" @?= 10,
      testCase "example3" $
        Day01.part2 "L70" @?= 1,
      testCase "example4" $ Day01.part2 "L50" @?= 1,
      testCase "example5" $ Day01.part2 "L50\nL5" @?= 1
    ]
