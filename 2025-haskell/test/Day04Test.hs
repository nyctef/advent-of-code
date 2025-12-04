module Day04Test where

import Data.Text (Text)
import Day04
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 4 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 13

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 4 - Part 2"
    [ testCase "example1" $ do
        let parsed = parseInput example1
        let parsed_ = either ignoreError id parsed
        part2 parsed_ @?= 43
    ]
