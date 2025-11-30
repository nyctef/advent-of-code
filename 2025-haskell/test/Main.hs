module Main where

import Test.Tasty

import qualified Day01Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ Day01Test.tests
  ]
