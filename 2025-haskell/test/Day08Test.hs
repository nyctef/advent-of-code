module Day08Test where

import Data.Text (Text)
import Day08
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  .......S.......
  ...............
  .......^.......
  ...............
  ......^.^......
  ...............
  .....^.^.^.....
  ...............
  ....^.^...^....
  ...............
  ...^.^...^.^...
  ...............
  ..^...^.....^..
  ...............
  .^.^.^.^.^...^.
  ...............
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 8 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 0

testPart2 :: Text -> Int -> IO ()
testPart2 input expected = do
  let parsed = parseInput input
  let parsed_ = either ignoreError id parsed
  part2 parsed_ @?= expected

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 8 - Part 2"
    [ testCase "example1" $
        testPart2 example1 0
    ]
