module Day11Test where

import Data.Text (Text)
import Day11
import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 =
  """
  aaa: you hhh
  you: bbb ccc
  bbb: ddd eee
  ccc: ddd eee fff
  ddd: ggg
  eee: out
  fff: out
  ggg: out
  hhh: ccc fff iii
  iii: out
  """

ignoreError :: b -> a
ignoreError = const $ error "couldn't parse"

test_part1 :: TestTree
test_part1 = testCase "Day 11 - Part 1" $ do
  let parsed = parseInput example1
  let parsed_ = either ignoreError id parsed
  part1 parsed_ @?= 5

example2 :: Text
example2 =
  """
  svr: aaa bbb
  aaa: fft
  fft: ccc
  bbb: tty
  tty: ccc
  ccc: ddd eee
  ddd: hub
  hub: fff
  eee: dac
  dac: fff
  fff: ggg hhh
  ggg: out
  hhh: out
  """

example3 :: Text
example3 =
  """
  svr: fft
  fft: dac
  dac: out
  """

example4 :: Text
example4 =
  """
  svr: fft
  fft: dac
  dac: aaa bbb
  aaa: out
  bbb: out
  """

example5 :: Text
example5 =
  """
  svr: fft
  fft: dac
  dac: aaa
  aaa: bbb ccc
  bbb: out
  ccc: out
  """

testPart2 :: Text -> Int -> IO ()
testPart2 input expected = do
  let parsed = parseInput input
  let parsed_ = either ignoreError id parsed
  part2 parsed_ @?= expected

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 11 - Part 2"
    [ testCase "example2" $ testPart2 example2 2,
      testCase "example3" $ testPart2 example3 1,
      testCase "example4" $ testPart2 example4 2,
      testCase "example5" $ testPart2 example5 2
    ]
