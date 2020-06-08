import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

import Data.Array
import Data.Bits
import Data.Char
import qualified Data.Heap as PQ
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Utils
import Debug.Trace

data BFSState = BFSState { pq :: PriorityQueue,
                           visited :: Map.Map Pos Int,
                           maze :: Maze,
                           nKeys :: Int
                         }
type PriorityQueue = PQ.MinPrioHeap Int Pos
type Pos = ((Int, Int), Int) -- x, y, keys
type VaultState = State BFSState (Maybe Int)
type Maze = Array (Int, Int) Char

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let maze = parseMaze input
      Just p1 = findShortestPath maze
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2

findShortestPath :: Maze -> Maybe Int
findShortestPath maze =
  let Just start = find isStart $ indices maze
      maze' = maze // [(start, '.')]
      pq = PQ.singleton (0, (start, zeroBits))
      v = Map.empty
      k = length $ filter isLower (elems maze')
  in evalState bfs (BFSState pq v maze' k)
  where isStart p = maze ! p == '@'


parseMaze :: [String] -> Maze
parseMaze inputs =
  let h = length inputs
      w = length $ head inputs

      parse _ _ [] = []
      parse i j ([]:rs) = parse (i + 1) 0 rs
      parse i j ((c:cs):rs) = ((i, j), c) : parse i (j + 1) (cs:rs)

  in array ((0, 0), (h - 1, w - 1)) $ parse 0 0 inputs

bfs = do
  pq <- gets pq
  n <- gets nKeys
  maze <- gets maze
  case PQ.view pq of
    Nothing -> return Nothing
    Just ((p, (pos, keys)), pq')
      | popCount keys == n -> return $ Just (p - 1)
      | otherwise -> putPQ pq' >> bfs' p (pos, keys)

bfs' :: Int -> ((Int, Int), Int) ->  VaultState
bfs' p loc@(pos, k) = do
  v <- gets visited
  maze <- gets maze
  let status = maze ! pos
  case (ord status, Map.member loc v, isLower status) of
    (46, True, _) -> updateV loc p >> bfs
    (46, False, _) -> updateV loc p >> addDirs loc p >> bfs
    (35, _, _) -> bfs
    (c, False, True) -> updateV (pos, withK c) p >> addDirs (pos, withK c) p >> bfs
    (c, False, False) -> if canPass c
                          then updateV loc p >> addDirs loc p >> bfs
                          else bfs
    _ -> bfs
  where withK c = setBit k (c - 97)
        canPass c = testBit k (c - 65)

updateV :: Pos -> Int -> State BFSState ()
updateV loc p = do
  v <- gets visited
  let v' = Map.insertWith min loc p v
  putV v'

addDirs :: Pos -> Int -> State BFSState ()
addDirs loc@(pos, k) p = do
  pq <- gets pq
  let loc' = map (plusLoc loc) ds
  putPQ $ foldl (ins p) pq loc'
  where ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        plusLoc ((y, x), k) (y', x') = ((y + y', x + x'), k)

ins :: Int -> PriorityQueue -> Pos -> PriorityQueue
ins p pq' loc = PQ.insert (p + 1, loc) pq'

putV :: Map.Map Pos Int -> State BFSState ()
putV v = modify (\s -> s {visited = v})

putPQ :: PriorityQueue -> State BFSState ()
putPQ q = modify (\s -> s {pq = q})
