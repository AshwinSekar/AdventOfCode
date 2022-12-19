import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Array
import           Data.Char
import qualified Data.Heap                as PQ
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import           Debug.Trace
import           Utils

data BFSState =
  BFSState
    { pq      :: PriorityQueue
    , visited :: Map.Map ((Int, Int), Int) Int
    , end     :: ((Int, Int), Maybe Int)
    , portals :: PortalMap
    , maze    :: Maze
    }

type PriorityQueue = PQ.MinPrioHeap Int ((Int, Int), Int)

type DonutState = State BFSState (Maybe Int)

type PortalMap = Map.Map (Int, Int) ((Int, Int), Int)

type Maze = Array (Int, Int) Char

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let maze = parseMaze input
      (start, end, portals) = parsePortal maze
      Just p1 = findShortestPath maze portals start (end, Nothing)
      Just p2 = findShortestPath maze portals start (end, Just 0)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

findShortestPath ::
     Maze -> PortalMap -> (Int, Int) -> ((Int, Int), Maybe Int) -> Maybe Int
findShortestPath maze portals start end =
  let pq = PQ.singleton (0, (start, 0))
      v = Map.empty
   in evalState bfs (BFSState pq v end portals maze)

parseMaze :: [String] -> Maze
parseMaze inputs =
  let h = length inputs
      w = length $ head inputs
      parse _ _ []          = []
      parse i j ([]:rs)     = parse (i + 1) 0 rs
      parse i j ((c:cs):rs) = ((i, j), c) : parse i (j + 1) (cs : rs)
   in array ((0, 0), (h - 1, w - 1)) $ parse 0 0 inputs

parsePortal :: Array (Int, Int) Char -> ((Int, Int), (Int, Int), PortalMap)
parsePortal grid =
  let ((0, 0), (h, w)) = bounds grid
      is = indices grid
      opens = filter (\p -> grid ! p == '.') is
      open = mapMaybe findV opens ++ mapMaybe findH opens
      openMap = Map.fromListWith (++) open
      (("AA", [(start, 1)]), openMap') = Map.deleteFindMin openMap
      (("ZZ", [(end, 1)]), openMap'') = Map.deleteFindMax openMap'
      portalPoints =
        map (\[(i, _), (j, l)] -> (i, (j, l))) $ Map.elems openMap''
      portals =
        portalPoints ++
        map (\[(i, l), (j, _)] -> (j, (i, l))) (Map.elems openMap'')
      findV (i, j) =
        let surroundings = [(i + 2, j), (i + 1, j), (i - 1, j), (i - 2, j)]
            alpha = filter isAlpha $ map (grid !) surroundings
         in case (i, alpha) of
              (i, [a, b])
                | i == 2 -> Just ([a, b], [((i, j), 1)])
                | i == h - 2 -> Just ([a, b], [((i, j), 1)])
                | otherwise -> Just ([a, b], [((i, j), -1)])
              _ -> Nothing
      findH (i, j) =
        let surroundings = [(i, j + 2), (i, j + 1), (i, j - 1), (i, j - 2)]
            alpha = filter isAlpha $ map (grid !) surroundings
         in case (j, alpha) of
              (j, [a, b])
                | j == 2 -> Just ([a, b], [((i, j), 1)])
                | j == w - 2 -> Just ([a, b], [((i, j), 1)])
                | otherwise -> Just ([a, b], [((i, j), -1)])
              _ -> Nothing
   in (start, end, Map.fromList portals)

bfs = do
  pq <- gets pq
  e <- gets end
  maze <- gets maze
  portals <- gets portals
  case (PQ.view pq, e) of
    (Nothing, _) -> return Nothing
    (Just ((p, (pos, _)), pq'), (e, Nothing))
      | pos == e -> return $ Just p
      | otherwise -> putPQ pq' >> bfs' p (pos, 0)
    (Just ((p, (pos, -1)), pq'), (_, Just _)) -> putPQ pq' >> bfs
    (Just ((p, loc), pq'), (e, Just l))
      | loc == (e, l) -> return $ Just p
      | otherwise -> putPQ pq' >> bfs' p loc

bfs' :: Int -> ((Int, Int), Int) -> DonutState
bfs' p loc@(pos, l) = do
  v <- gets visited
  maze <- gets maze
  let status = maze ! pos
  case (status, Map.member loc v) of
    ('.', True)  -> updateV loc p >> bfs
    ('.', False) -> updateV loc p >> addDirs loc p >> bfs
    _            -> bfs

updateV :: ((Int, Int), Int) -> Int -> State BFSState ()
updateV loc p = do
  v <- gets visited
  let v' = Map.insertWith min loc p v
  putV v'

addDirs :: ((Int, Int), Int) -> Int -> State BFSState ()
addDirs loc@(pos, l) p = do
  pq <- gets pq
  portals <- gets portals
  let loc' = map (plusLoc loc) ds
      loc'' =
        if Map.member pos portals
          then plusLevel l (portals Map.! pos) : loc'
          else loc'
  putPQ $ foldl (ins p) pq loc''
  where
    ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    plusLoc ((y, x), l) (y', x') = ((y + y', x + x'), l)
    plusLevel l' (pos, l) = (pos, l + l')

ins :: Int -> PriorityQueue -> ((Int, Int), Int) -> PriorityQueue
ins p pq' loc = PQ.insert (p + 1, loc) pq'

putV :: Map.Map ((Int, Int), Int) Int -> State BFSState ()
putV v = modify (\s -> s {visited = v})

putPQ :: PriorityQueue -> State BFSState ()
putPQ q = modify (\s -> s {pq = q})
