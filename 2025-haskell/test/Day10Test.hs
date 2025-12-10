module Day10Test where

import Data.Text (Text)
import Day10
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
  [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
  [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 10 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 7

testPart2 :: Text -> Int -> IO ()
testPart2 input expected = do
  let parsed = parseInput input
  let parsed_ = either ignoreError id parsed
  part2 parsed_ @?= expected

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 10 - Part 2"
    [ testCase "example1" $ testPart2 example1 0,
    ]
