module Day05Test where

import Data.Text (Text)
import Day05
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 5 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 3

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 5 - Part 2"
    [ testCase "example1" $ do
        let parsed = parseInput example1
        let parsed_ = either ignoreError id parsed
        part2 parsed_ @?= 14
    ]
