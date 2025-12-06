module Day06 (solve, part1, part2, parseInput1, parseInput2) where

import Data.Char (isDigit)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import InputFetcher (getInput)

transposeVec :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)
transposeVec = V.fromList . map V.fromList . transpose . map V.toList . V.toList

rotateLeftVec :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
rotateLeftVec vecs
  | V.null vecs || V.null (V.head vecs) = V.empty
  | otherwise = V.generate nCols $ \col ->
      V.generate nRows $ \row ->
        vecs V.! row V.! (nCols - 1 - col)
  where
    nRows = V.length vecs
    nCols = V.length (V.head vecs)

type Input1 = (Vector (Vector Int), Vector Text)

type Input2 = Vector (Vector Char)

doMath :: Text -> V.Vector Int -> Int
doMath "*" xs = foldl (*) 1 xs
doMath "+" xs = foldl (+) 0 xs
doMath _ _ = undefined

doMath2 :: Char -> [Int] -> Int
doMath2 '*' xs = foldl (*) 1 xs
doMath2 '+' xs = foldl (+) 0 xs
doMath2 _ _ = undefined


part1 :: Input1 -> Int
part1 input =
  let (nums, ops) = input
      height = V.length nums
      length = V.length (nums V.! 0)
      opsLength = V.length ops
      cols = transposeVec nums
      results = V.map (\(c, o) -> doMath o c) (cols `V.zip` ops)
   in {- traceShow (cols, results) -} V.sum results

isNotBlank :: Vector Char -> Bool
isNotBlank = V.any (\x -> x /= ' ')

splitMaybeOp :: (Text, Text) -> (Int, Maybe Char)
splitMaybeOp (l, r) = (read $ T.unpack l_, if r_ /= "" then Just $ T.head r_ else Nothing)
  where
    l_ = T.strip l
    r_ = T.strip r

accu :: (Int, [Int]) -> (Int, Maybe Char)   -> (Int, [Int])
accu (t, pending) (n, (Just op))  = (t + doMath2 op (pending ++ [n]), [])
accu (t, pending) (n, Nothing)  = (t, pending ++ [n])

traceAcc :: (Show a, Show b) => (a -> b -> a) -> (a -> b -> a)
traceAcc f x n =
  let result = f x n
   in traceShow result result

part2 :: Input2 -> Int
part2 input = result
  where
    rtl = rotateLeftVec $ {- traceShow input -} input
    withoutBlanks = V.filter isNotBlank rtl
    lines = V.map (splitMaybeOp . (T.span isDigit) . T.strip . T.pack . V.toList) withoutBlanks
    (total, pending) = {- traceShow lines -} (foldl ({- traceAcc -} accu) (0, []) lines)
    result = total

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput1 :: Text -> Either String Input1
parseInput1 i =
  let (numsT, opsT) = T.breakOnEnd "\n" $ T.strip i
      numsL = T.lines $ T.strip numsT
      nums = V.fromList $ map (\l -> V.fromList $ map (read . T.unpack) (T.words l)) numsL
      ops = V.fromList $ T.words opsT
   in Right (nums, ops)

parseInput2 :: Text -> Either String Input2
parseInput2 i =
  let lines = V.fromList $ T.splitOn "\n" $ T.dropAround (=='\n') i
      chars = V.map (V.fromList . T.unpack) lines
   in {- traceShow (V.length chars, V.map V.length chars) -} Right chars

solve :: IO ()
solve = do
  input <- getInput 2025 6
  let parsed1 = parseInput1 input
  let parsed2 = parseInput2 input
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed1)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed2)
