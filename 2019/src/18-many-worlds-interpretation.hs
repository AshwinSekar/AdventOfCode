import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

import Data.Array
import Data.Bits
import Data.Char
import Data.Function
import qualified Data.Heap as PQ
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Utils
import Debug.Trace

data BFSState q = BFSState { pq :: PriorityQueue q
                           , visited :: Map.Map Pos Int
                           , maze :: Maze
                           , nKeys :: Int
                           }
type PriorityQueue a = PQ.MinPrioHeap Int a
type Pos = ((Int, Int), Int) -- x, y, keys
type VaultState s = State (BFSState s) (Maybe Int)
type Maze = Array (Int, Int) Char

type RoboPos = (Int, Int)
type Robots = ([RoboPos], Int) -- robots, keys
type RobotBFSState = BFSState Robots
type RobotState s = State RobotBFSState s


main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let maze = parseMaze input
      Just p1 = findShortestPath maze
      Just p2 = findShortestRobotPath maze
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2


findShortestPath :: Maze -> Maybe Int
findShortestPath maze =
  let Just start = find isStart $ indices maze
      maze' = maze // [(start, '.')]
      pq = PQ.singleton (0, (start, zeroBits))
      v = Map.empty
      k = length $ filter isLower (elems maze')
  in evalState bfs (BFSState pq v maze' k)
  where isStart p = maze ! p == '@'

findShortestRobotPath :: Maze -> Maybe Int
findShortestRobotPath maze =
  let Just start = find isStart $ indices maze
      maze' = start : findDirs start
            & map (, '#')
            & (maze //)
      pq = PQ.singleton (0, (findStarts start, zeroBits))
      v = Map.empty
      k = length $ filter isLower (elems maze')
  in evalState roboBFS $ BFSState pq v maze' k
  where findDirs (sy, sx) = [(sy + 1, sx), (sy - 1, sx), (sy, sx + 1), (sy, sx - 1)]
        findStarts (sy, sx) = [(sy + 1, sx + 1), (sy - 1, sx - 1), (sy - 1, sx + 1), (sy + 1, sx - 1)]
        isStart p = maze ! p == '@'

parseMaze :: [String] -> Maze
parseMaze inputs =
  let h = length inputs
      w = length $ head inputs

      parse _ _ [] = []
      parse i j ([]:rs) = parse (i + 1) 0 rs
      parse i j ((c:cs):rs) = ((i, j), c) : parse i (j + 1) (cs:rs)

  in array ((0, 0), (h - 1, w - 1)) $ parse 0 0 inputs

bfs :: VaultState Pos
bfs = do
  pq <- gets pq
  n <- gets nKeys
  maze <- gets maze
  case PQ.view pq of
    Nothing -> return Nothing
    Just ((p, (pos, keys)), pq')
      | popCount keys == n -> return $ Just (p - 1)
      | otherwise -> putPQ pq' >> bfs' p (pos, keys)

bfs' :: Int -> ((Int, Int), Int) ->  VaultState Pos
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

roboBFS :: RobotState (Maybe Int)
roboBFS = do
  pq <- gets pq
  n <- gets nKeys
  case PQ.view pq of
    Nothing -> return Nothing
    Just ((p, s@(poss, k)), pq')
      | popCount k == n -> return $ Just p
      | otherwise -> putPQ pq' >> roboBFS' p k [] poss >> roboBFS

roboBFS' :: Int -> Int -> [RoboPos] -> [RoboPos] -> RobotState ()
roboBFS' p k poss [] = return ()
roboBFS' p k done (pos : poss') = do
  v <- gets visited
  unless (Map.member loc v) $ updateV loc p >> addRoboDirs p others pos k
  roboBFS' p k done' poss'
  where loc = (pos, k)
        others = done ++ poss'
        done' = pos : done

updateV :: Pos -> Int -> State (BFSState q) ()
updateV loc p = do
  v <- gets visited
  putV $ Map.insertWith min loc p v

addRoboDirs :: Int -> [RoboPos] -> RoboPos -> Int -> RobotState ()
addRoboDirs p others (y, x) k = do
  m <- gets maze
  v <- gets visited
  pq <- gets pq
  map add ds
    & filter (notWall m)
    & filter (canOpen m)
    & filter (notVisited v)
    & map (pickupKeys m)
    & foldl (ins p) pq
    & putPQ
  where ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        add (i, j) = ((y + i, x + j) : others, k)
        notWall m (pos : _, _) = m ! pos /= '#'
        notVisited v (pos : _, k) = not $ Map.member (pos, k) v

canOpen m (pos : _, k) =
  let status = m ! pos
      c = ord status
  in not (isUpper status) || testBit k (c - 65)

pickupKeys m loc@(pos : poss, k) =
  let status = m ! pos
      c = ord status
  in if isLower status
      then (pos : poss, setBit k (c - 97))
      else loc

addDirs :: Pos -> Int -> State (BFSState Pos) ()
addDirs loc@(pos, k) p = do
  pq <- gets pq
  let loc' = map (plusLoc loc) ds
  putPQ $ foldl (ins p) pq loc'
  where ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        plusLoc ((y, x), k) (y', x') = ((y + y', x + x'), k)

ins :: Int -> PriorityQueue q -> q -> PriorityQueue q
ins p pq' loc = PQ.insert (p + 1, loc) pq'

putV :: Map.Map Pos Int -> State (BFSState q) ()
putV v = modify (\s -> s {visited = v})

putPQ :: PriorityQueue q -> State (BFSState q) ()
putPQ q = modify (\s -> s {pq = q})

showMaze :: Maze -> Int -> Robots -> String
showMaze maze p (pos, k) =
    let maze' = map (, '@') pos
              & (maze //)
              & elems
              & slices 7
    in show p ++ "\nKey: " ++ show k ++ "\n" ++ unlines maze'
