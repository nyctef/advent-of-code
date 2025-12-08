module Day08Test where

import Data.Text (Text)
import Day08
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  162,817,812
  57,618,57
  906,360,560
  592,479,940
  352,342,300
  466,668,158
  542,29,236
  431,825,988
  739,650,466
  52,470,668
  216,146,977
  819,987,18
  117,168,530
  805,96,715
  346,949,466
  970,615,88
  941,993,340
  862,61,35
  984,92,344
  425,690,689
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
