module Day11 (solve, part1, part2, parseInput) where

import Control.Arrow (left)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Data.List (intercalate)
import InputFetcher (getInput)
import Text.Parsec hiding (Line, count, getInput)
import Text.Parsec.Text (Parser)
import Text.Printf(printf)
import Data.HashMap.Strict(HashMap(..), (!))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet(HashSet(..))
import qualified Data.HashSet as HashSet
import Data.Either (fromRight)
import System.IO (hFlush, stdout)
import Data.Heap(MinPrioHeap(..))
import qualified Data.Heap as H
import Control.Monad(replicateM)

data Connection = Connection { getFrom :: Text , getTo :: [Text] }
instance Show Connection where
  show (Connection from to) = printf "%s -> %s" from ("," `T.intercalate` to)

type Input = [Connection]

connectionP :: Parser Connection
connectionP = do
  from <- many1 alphaNum
  _ <- char ':'
  _ <- char ' '
  to <- (many1 alphaNum) `sepBy` char ' '
  let to' = map T.pack to
  return $ Connection (T.pack from) to'

connectionsP :: Parser [Connection]
connectionsP = connectionP `sepBy` char '\n'

-- like traceShow, except that it doesn't
_traceShow :: a -> b -> b
_traceShow = seq

toMap :: [Connection] -> HashMap Text [Text]
toMap cs = HashMap.fromList $ map (\c -> (getFrom c, getTo c)) cs

countPaths :: HashMap Text [Text] -> Text -> Text -> Int
countPaths map start end
  | start == end = 1
  | start == "out" = 0
  | otherwise = sum $ fmap (\n -> countPaths map n end) (map ! _traceShow (start, end) start)

-- counts of paths to each node, queue of nodes to process next
type State2 = (HashMap Text Int, MinPrioHeap Int Text)

{-
countPaths2 :: HashMap Text [Text] -> Text -> Text -> Int
countPaths2 conns start end = result
  where
    go :: State2 -> Int
    go (counts, queue) =
      case Heap.view queue of
        Nothing -> counts ! end
        Just ((prio, current), queue') -> go (newCounts, newQueue)
          where
            -- we assume that once we've reached a node, we've fully processed all nodes
            -- leading into this one (TODO: verify)
            -- for nodes we can reach from current:
            -- - we add this node's count to each of them
            -- - 
          
          

    result = go (HashMap.empty, H.singleton (0, start))
-}

getDistances :: Input -> HashMap (Text, Text) Int
getDistances i = result
  where
    conns = [((getFrom f, t), 1) | f <- i, t <- (getTo f)]
    seed = HashMap.fromList conns
    nodes = "out" : map getFrom i
    iterations :: [(Text,Text,Text)]
    iterations = [(i,j,k) | i<-nodes, j<-nodes, k<-nodes]
    get = HashMap.findWithDefault 0 
    result = foldr
      (\(i,j,k) m -> HashMap.insertWith (+) (i, j) ((get (i, k) m) * (get (k, j) m)) m)
      seed iterations

topologicalSort :: Text -> HashMap Text [Text] -> [Text]
topologicalSort start graph = dfs HashSet.empty start []
  where
    dfs visited next acc
      | HashSet.member next visited = acc
      | otherwise =
          let nextVisited = HashSet.insert next visited
              children = HashMap.findWithDefault [] next graph
              successors = foldl' (\a c -> dfs nextVisited c a) acc children
          in next : successors


countPaths2 :: Text -> Text -> HashMap Text [Text] -> Int
countPaths2 start end map = result
  where
    sorted = traceShowId $ topologicalSort start map
    counts = traceShowId $ foldl' updateCounts (HashMap.singleton start 1) sorted
    result = traceShowId $ HashMap.findWithDefault 0 end counts
    updateCounts counts node =
      let children = HashMap.findWithDefault [] node map
          nodeCount = HashMap.findWithDefault 0 node counts
      in foldl' (\cs child -> HashMap.insertWith (+) child nodeCount cs) counts children

part1 :: Input -> Int
part1 input = result
  where
    map = toMap input
    result = countPaths map "you" "out"

part2 :: Input -> Int
part2 input = result
  where
    map = toMap input
    a = countPaths2 "svr" "fft" map
    b = countPaths2 "fft" "dac" map
    c = countPaths2 "dac" "out" map
    -- d = countPaths2 map "svr" "dac" 
    -- e = countPaths2 map "dac" "fft" 
    -- f = countPaths2 map "fft" "out" 
    -- result = traceShow (filter (\(_, x) -> x /= 0) $ HashMap.toList  dists, a, b, c) (a*b*c)-- + (d*e*f)
    result = (a*b*c)

{-
toDot :: Input -> String
toDot i = printf "digraph { \n %s \n }" nodeStr
  where
    nodes = concatMap (\c -> [(getFrom c, t) | t<-getTo c]) i
    nodeStrs = map (\(f, t) -> printf "%s -> %s" f t) nodes
    nodeStr = intercalate "\n" nodeStrs
-}

tshow :: (Show a) => a -> Text
tshow = T.pack . show

parseInput :: Text -> Either String Input
parseInput i = left show $ parse connectionsP "" $ T.strip i

solve :: IO ()
solve = do
  input <- getInput 2025 11
  let parsed = parseInput input
  -- putStrLn $ toDot $ (fromRight [] parsed)
  -- hFlush stdout
  -- TIO.putStrLn $ "Input: " <> tshow parsed
  TIO.putStrLn $ "  Part 1: " <> tshow (part1 <$> parsed)
  TIO.putStrLn $ "  Part 2: " <> tshow (part2 <$> parsed)
