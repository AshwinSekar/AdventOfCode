import           Control.Applicative  (Alternative ((<|>)), liftA2)
import           Data.Char            (isAsciiUpper, ord)
import           Data.Complex         (Complex ((:+)), imagPart, realPart)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import qualified Data.Heap            as PQ
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           Text.Megaparsec      (MonadParsec (try), choice, some)
import           Text.Megaparsec.Char (char, newline, printChar)
import           Utils                (Parser, count, decimal, fourDirs, getFile, gridMap, lexeme,
                                       parseFile, psum, slices, symbol, word)

type Grid = Map.Map (Integer, Integer) Integer

type PriorityQueue = PQ.MinPrioHeap Integer (Integer, Integer)

dijkstra :: Grid -> Grid -> PriorityQueue -> Grid
dijkstra _ v (PQ.view -> Nothing) = v
dijkstra grid v (PQ.view -> Just ((s, p), pq))
  | p `Map.member` v = dijkstra grid v pq
  | otherwise = dijkstra grid v' pq'
  where
    curElevation = grid ! p
    v' = Map.insert p s v
    neighs =
      filter (\p' -> grid ! p' <= curElevation + 1) $
      filter (`Map.member` grid) $ filter (`Map.notMember` v) (map (psum p) fourDirs)
    pq' = foldl (\q x -> PQ.insert (s + 1, x) q) pq neighs

main :: IO ()
main = do
  hills <- Map.map (\c -> toInteger $ ord c - 97) . gridMap <$> getFile "data/12-puzzle-input"
  let Just (start, _) = find ((== -14) . snd) $ Map.assocs hills
      Just (end, _) = find ((== -28) . snd) $ Map.assocs hills
      elevations = Map.insert start 0 $ Map.insert end 25 hills
      starts = map fst $ filter ((== 0) . snd) $ Map.assocs elevations
      distances = dijkstra elevations Map.empty $ PQ.singleton (0, start)
      trails = dijkstra elevations Map.empty $ PQ.fromList (map (0, ) starts)
      p1 = distances ! end
      p2 = trails ! end
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
