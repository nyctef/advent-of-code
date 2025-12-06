module Day06Test where

import Data.Text (Text)
import Day06
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  123 328  51 64 
   45 64  387 23 
    6 98  215 314
  *   +   *   +  
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 6 - Part 1" $ do
  let parsed = parseInput1 example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 4277556

testPart2 :: Text -> Int -> IO ()
testPart2 input expected = do
  let parsed = parseInput2 input
  let parsed_ = either ignoreError id parsed
  part2 parsed_ @?= expected

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 6 - Part 2"
    [ testCase "example1" $
        testPart2 example1 3263827
    ]
