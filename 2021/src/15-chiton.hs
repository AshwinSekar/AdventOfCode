import Control.Applicative (liftA2, (<|>))
import Data.Char (digitToInt)
import Data.Function ((&))
import qualified Data.Heap as PQ
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils

main :: IO ()
main = do
  cave <-
    Map.map (toInteger . digitToInt) . gridMap
      <$> getFile "data/15-puzzle-input"
  let v = dijkstra cave Map.empty $ PQ.singleton (0, (0, 0))
      goal@(w, h) = fst $ Map.findMax cave
      tileate :: (Integer, Integer) -> Map.Map (Integer, Integer) Integer
      tileate (x, y) =
        Map.mapKeysMonotonic (psum (x * (w + 1), y * (h + 1))) cave
          & Map.map (\c -> (c + x + y) `mod` 9)
          & Map.map
            ( \c ->
                if c == 0
                  then 9
                  else c
            )
      caveTiles = map tileate $ liftA2 (,) [0 .. 4] [0 .. 4]
      cave' = Map.unions caveTiles
      v' = dijkstra cave' Map.empty $ PQ.singleton (0, (0, 0))
      goal' = fst $ Map.findMax cave'
  putStrLn $ "Part 1: " ++ show (v ! goal)
  putStrLn $ "Part 2: " ++ show (v' ! goal')

dijkstra ::
  Map.Map (Integer, Integer) Integer ->
  Map.Map (Integer, Integer) Integer ->
  PQ.MinHeap (Integer, (Integer, Integer)) ->
  Map.Map (Integer, Integer) Integer
dijkstra _ v (PQ.view -> Nothing) = v
dijkstra grid v (PQ.view -> Just ((s, p), pq))
  | p `Map.member` v = dijkstra grid v pq
  | otherwise = dijkstra grid v' pq'
  where
    v' = Map.insert p s v
    neighs =
      filter (`Map.member` grid) $
        filter (`Map.notMember` v) (map (psum p) fourDirs)
    pq' = foldl (\q x -> PQ.insert (s + grid ! x, x) q) pq neighs
