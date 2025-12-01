{-# LANGUAGE MultilineStrings #-}

module Day01Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Day01

tests :: TestTree
tests = testGroup "Day 1"
  [ testCase "Part 1 - placeholder" $ do
      Day01.part1 """
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
      """ @?= 3

  , testCase "Part 2 - placeholder" $ do
      Day01.part2 "" @?= 0
  ]
