module GridRC (GridRC (numCols, numRows), points, get, find, findMany, parse, rewrite) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import PointRC (PointRC (..))

data GridRC = GridRC {grid :: HashMap PointRC Char, numCols :: Int, numRows :: Int}

points :: GridRC -> [PointRC]
points g = [PointRC r c | r <- [0 .. numRows g], c <- [0 .. numCols g]]

get :: PointRC -> GridRC -> Maybe Char
get p g = HashMap.lookup p $ grid g

find :: Char -> GridRC -> Maybe PointRC
find c g = listToMaybe $ fmap fst $ filter ((== c) . snd) $ HashMap.toList $ grid g

findMany :: Char -> GridRC -> [PointRC]
findMany c g = fmap fst $ filter ((== c) . snd) $ HashMap.toList $ grid g

parse :: Text -> Either String GridRC
parse i =
  let ls = T.lines i
      cols = map (zip [0 ..] . T.unpack) ls
      rows = zip [0 ..] cols
      cells = [(PointRC r c, val) | (r, cs) <- rows, (c, val) <- cs]
      hashmap = HashMap.fromList cells
   in Right $ GridRC hashmap (length $ fromJust $ listToMaybe cols) (length cols)

rewrite :: (PointRC -> Char -> Char) -> GridRC -> GridRC
rewrite f g = GridRC (HashMap.mapWithKey f $ grid g) (numCols g) (numRows g)

instance Show GridRC where
  show g =
    unlines
      [ [fromMaybe ' ' $ get (PointRC r c) g | c <- [0 .. numCols g - 1]]
      | r <- [0 .. numRows g - 1]
      ]
