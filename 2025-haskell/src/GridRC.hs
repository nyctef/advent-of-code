module GridRC (GridRC(numCols, numRows), points, get, find, parse) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import PointRC(PointRC(..))

data GridRC = GridRC {grid :: HashMap PointRC Char, numCols :: Int, numRows :: Int} deriving (Show)


points :: GridRC -> [PointRC]
points g = [PointRC r c | r <- [0 .. numRows g], c <- [0 .. numCols g]]

get :: GridRC -> PointRC -> Maybe Char
get g p = HashMap.lookup p $ grid g

find :: GridRC -> Char -> Maybe PointRC
find g c = listToMaybe $ fmap fst $ filter ((== c) . snd) $ HashMap.toList $ grid g

parse :: Text -> Either String GridRC
parse i =
  let ls = T.lines i
      cols = map (zip [0 ..] . T.unpack) ls
      rows = zip [0 ..] cols
      cells = [(PointRC r c, val) | (r, cs) <- rows, (c, val) <- cs]
      hashmap = HashMap.fromList cells
   in Right $ GridRC hashmap (length $ fromJust $ listToMaybe cols) (length cols)
