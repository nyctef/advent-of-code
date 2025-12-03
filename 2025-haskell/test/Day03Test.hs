module Day03Test where

import Data.Text (Text)
import Day03
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either

example1 :: Text
example1 =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111
  """

ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 3 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 357

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 3 - Part 2"
    [ testCase "example1" $ do
        let parsed = parseInput example1
        let parsed_ = either ignoreError id parsed
        part2 parsed_ @?= 3121910778619
    ]
