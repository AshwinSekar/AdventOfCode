import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

import Data.Array
import Data.Char
import qualified Data.Heap as PQ
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Utils
import Debug.Trace

data BFSState = BFSState { pq :: PriorityQueue,
                             visited :: Map.Map (Int, Int) Int,
                             end :: (Int, Int)
                           }
type PriorityQueue = PQ.MinPrioHeap Int (Int, Int)
type DonutState = State BFSState (Maybe Int)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let grid = parseGrid input
      (start, end, portals) = parsePortal grid
      Just p1 = findShortestPath grid portals start end
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ p2

findShortestPath :: Array (Int, Int) Char -> Map.Map (Int, Int) (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe Int
findShortestPath maze portals start end =
  let pq = PQ.singleton (0, start)
      v = Map.empty
  in evalState (bfs maze portals) (BFSState pq v end)


parseGrid :: [String] -> Array (Int, Int) Char
parseGrid inputs =
  let h = length inputs
      w = length $ head inputs

      parse _ _ [] = []
      parse i j ([]:rs) = parse (i + 1) 0 rs
      parse i j ((c:cs):rs) = ((i, j), c) : parse i (j + 1) (cs:rs)

  in array ((0, 0), (h - 1, w - 1)) $ parse 0 0 inputs

parsePortal :: Array (Int, Int) Char -> ((Int, Int), (Int, Int), Map.Map (Int, Int) (Int, Int))
parsePortal grid =
  let is = indices grid
      opens = filter (\p -> grid ! p == '.') is
      open = mapMaybe findV opens ++ mapMaybe findH opens
      openMap = Map.fromListWith (++) open
      (("AA", [start]), openMap') = Map.deleteFindMin openMap
      (("ZZ", [end]), openMap'') = Map.deleteFindMax openMap'
      portalPoints = map (\[i, j] -> (i, j)) $ Map.elems openMap''
      portals = portalPoints ++ map (\(i, j) -> (j, i)) portalPoints

      findV (i, j) =
        case (grid ! (i + 2, j), grid ! (i + 1, j), grid ! (i - 1, j), grid ! (i - 2, j)) of
          (_, '.', '.', _) ->  Nothing
          (_, '.', '#', _) ->  Nothing
          (_, '#', '.', _) ->  Nothing
          (_, '#', '#', _) ->  Nothing
          (a, b, '.', _) ->  Just ([a, b], [(i, j)])
          (_, '.', a, b) ->  Just ([a, b], [(i, j)])

      findH (i, j) =
        case (grid ! (i, j + 2), grid ! (i, j + 1), grid ! (i, j - 1), grid ! (i, j - 2)) of
          (_, '.', '.', _) ->  Nothing
          (_, '.', '#', _) ->  Nothing
          (_, '#', '.', _) ->  Nothing
          (_, '#', '#', _) ->  Nothing
          (a, b, '.', _) ->  Just ([a, b], [(i, j)])
          (_, '.', a, b) ->  Just ([a, b], [(i, j)])

  in (start, end, Map.fromList portals)

bfs maze portals = do
  pq <- gets pq
  e <- gets end
  case PQ.view pq of
    Nothing -> return Nothing
    Just ((p, pos), pq')
      | pos == e -> return $ Just p
      | otherwise -> putPQ pq' >> bfs' maze portals p pos

bfs' :: Array (Int, Int) Char -> Map.Map (Int, Int) (Int, Int) -> Int -> (Int, Int) -> DonutState
bfs' maze portals p pos = do
  v <- gets visited
  let status = maze ! pos
  case (status, Map.member pos v) of
    ('.', True) -> updateV pos p >> bfs maze portals
    ('.', False) -> updateV pos p >> addDirs portals pos p >> bfs maze portals
    _ -> bfs maze portals

updateV :: (Int, Int) -> Int -> State BFSState ()
updateV pos p = do
  v <- gets visited
  let v' = Map.insertWith min pos p v
  putV v'

addDirs :: Map.Map (Int, Int) (Int, Int) -> (Int, Int) -> Int -> State BFSState ()
addDirs portals pos p = do
  pq <- gets pq
  let pos' = map (plusTuple pos) ds
      pos'' = if Map.member pos portals
                then portals Map.! pos : pos'
                else pos'
  putPQ $ foldl (ins p) pq pos''
  where ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        plusTuple (y, x) (y', x') = (y + y', x + x')

ins :: Int -> PriorityQueue -> (Int, Int) -> PriorityQueue
ins p pq' pos = PQ.insert (p + 1, pos) pq'


putV :: Map.Map (Int, Int) Int -> State BFSState ()
putV v = modify (\s -> s {visited = v})

putPQ :: PriorityQueue -> State BFSState ()
putPQ q = modify (\s -> s {pq = q})
