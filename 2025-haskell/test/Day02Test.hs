{-# LANGUAGE MultilineStrings #-}

module Day02Test where

import qualified Day02
import Test.Tasty
import Test.Tasty.HUnit

example1 =
  """
  11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
  1698522-1698528,446443-446449,38593856-38593862,565653-565659,
  824824821-824824827,2121212118-2121212124
  """

test_isInvalid1 :: TestTree
test_isInvalid1 =
  testGroup
    "isInvalid1"
    [ testCase "1" $ Day02.isInvalid1 1 @?= False,
      testCase "11" $ Day02.isInvalid1 11 @?= True,
      testCase "111" $ Day02.isInvalid1 111 @?= False,
      testCase "12" $ Day02.isInvalid1 12 @?= False,
      testCase "121212" $ Day02.isInvalid1 121212 @?= False
    ]

test_isInvalid2 :: TestTree
test_isInvalid2 =
  testGroup
    "isInvalid2"
    [ testCase "1" $ Day02.isInvalid2 1 @?= False,
      testCase "11" $ Day02.isInvalid2 11 @?= True,
      testCase "111" $ Day02.isInvalid2 111 @?= True,
      testCase "112" $ Day02.isInvalid2 112 @?= False,
      testCase "12" $ Day02.isInvalid2 12 @?= False,
      testCase "121212" $ Day02.isInvalid2 121212 @?= True
    ]

test_part1 :: TestTree
test_part1 = testCase "Day 1 - Part 1" $ do
  Day02.part1
    example1
    @?= 1227775554

test_part2 :: TestTree
test_part2 =
  testGroup
    "Day 1 - Part 2"
    [ testCase "example1" $
        Day02.part2 example1 @?= 4174379265
    ]
