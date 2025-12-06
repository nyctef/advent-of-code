module Day06 (solve, part1, part2, parseInput) where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import InputFetcher (getInput)
import qualified Data.Vector as V
import Data.Vector (Vector)


transposeVec :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)
transposeVec = V.fromList . map V.fromList . transpose . map V.toList . V.toList

type Input = (Vector (Vector Int), Vector Text)

doMath :: Text -> V.Vector Int -> Int
doMath "*" xs = foldl (*) 1 xs
doMath "+" xs = foldl (+) 0 xs
doMath _ _ = undefined

part1 :: Input -> Int
part1 input = let
    (nums, ops) = input
    height = V.length nums
    length = V.length (nums V.! 0)
    opsLength = V.length ops
    cols = transposeVec nums
    results = V.map (\(c, o) -> doMath o c) (cols `V.zip` ops) 
  in {- traceShow (cols, results) -} V.sum results


part2 :: Input -> Int
part2 input = 0

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i =
  let (numsT, opsT) = T.breakOnEnd "\n" $ T.strip i
      numsL = T.lines $ T.strip numsT
      nums = V.fromList $ map (\l -> V.fromList $ map (read . T.unpack) (T.words l)) numsL
      ops = V.fromList $ T.words opsT

   in Right (nums, ops)

solve :: IO ()
solve = do
  input <- getInput 2025 6
  let parsed = parseInput input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
