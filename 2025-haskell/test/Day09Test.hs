module Day09Test where

import Data.Text (Text)
import Day09
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  7,1
  11,1
  11,7
  9,7
  9,5
  2,5
  2,3
  7,3
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 9 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 50

testPart2 :: Text -> Int -> IO ()
testPart2 input expected = do
  let parsed = parseInput input
  let parsed_ = either ignoreError id parsed
  part2 parsed_ @?= expected

{-
  0123456789
0 ...........
1 .#--##--#.
2 .|..||..|
3 .|..##..|.
4 .|......|.
5 .#------#.
6 .......... -}
example2 :: Text
example2 =
  """
  1,1
  4,1
  4,3
  5,3
  5,1
  8,1
  8,5
  1,5
  """

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 9 - Part 2"
    [ testCase "example1" $ testPart2 example1 24,
      testCase "example2 (incorrect, should be 40)" $ testPart2 example2 25
    ]
