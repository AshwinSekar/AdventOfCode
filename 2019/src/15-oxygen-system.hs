import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.ST.Trans
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Array.ST
import qualified Data.Heap                      as PQ
import           Data.List
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import           Debug.Trace
import           System.Console.ANSI
import           System.IO
import           System.Random
import           Utils

-- prog, instruction pointer, relative base, inputs
data ProgState s =
  ProgState
    { ip     :: Integer
    , base   :: Integer
    , mov    :: Integer
    , dir    :: Integer
    , pos    :: (Int, Int)
    , grid   :: STArray s (Int, Int) (Maybe Integer)
    , inputF :: InputF s
    , oxy    :: Maybe (Int, Int)
    }

data BFSState s =
  BFSState
    { maze    :: STArray s (Int, Int) (Maybe Integer)
    , pq      :: PriorityQueue
    , visited :: Map.Map (Int, Int) Int
    }

type IntProg s = STArray s Integer Integer

type MazeState s = StateT (ProgState s) (STT s IO) ()

type FloodState s = StateT (BFSState s) (STT s IO) ()

type PriorityQueue = PQ.MinPrioHeap Int (Int, Int)

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

type InputF s = StateT (ProgState s) (STT s IO) Integer

liftIO :: IO a -> StateT (ProgState s) (STT s IO) a
liftIO = lift . lift

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/15-puzzle-input"
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  runDroid intprog

bfs :: ProgState s -> STT s IO ()
bfs state = do
  let g = grid state
      Just o = oxy state
      pq = PQ.singleton (0, o)
      v = Map.empty
  v' <- visited <$> execStateT bfs' (BFSState g pq v)
  let minutes = maximum $ Map.elems v'
  lift $ setCursorPosition 70 0
  lift $ putStrLn ("Part 2: " ++ show minutes)

bfs' :: FloodState s
bfs' = do
  pq <- gets pq
  case PQ.view pq of
    Nothing              -> return ()
    Just ((p, pos), pq') -> putPQ pq' >> bfs'' p pos

bfs'' :: Int -> (Int, Int) -> FloodState s
bfs'' p pos = do
  v <- gets visited
  g <- gets maze
  status <- lift $ readArray g pos
  case (status, Map.member pos v) of
    (Just 0, _) -> bfs'
    (_, True)   -> updateV pos p >> bfs'
    (_, False)  -> updateV pos p >> addDirs pos p >> bfs'

updateV :: (Int, Int) -> Int -> FloodState s
updateV pos p = do
  v <- gets visited
  let v' = Map.insertWith min pos p v
  putV v'

addDirs :: (Int, Int) -> Int -> FloodState s
addDirs pos p = do
  pq <- gets pq
  let pos' = map (applyDir pos) ds
      pq' = foldl (ins p) pq pos'
  putPQ pq'
  where
    ds = [1, 2, 3, 4]

ins :: Int -> PriorityQueue -> (Int, Int) -> PriorityQueue
ins p pq' pos = PQ.insert (p + 1, pos) pq'

putV v = modify (\s -> s {visited = v})

putPQ q = modify (\s -> s {pq = q})

runDroid :: [Integer] -> IO ()
runDroid prog = do
  clearScreen
  hideCursor
  runSTT $ runProgram prog getAi >>= bfs

applyDir (posx, posy) d =
  case d of
    1 -> (posx, posy - 1)
    2 -> (posx, posy + 1)
    3 -> (posx - 1, posy)
    4 -> (posx + 1, posy)

turnR 1 = 4
turnR 2 = 3
turnR 3 = 1
turnR 4 = 2

turnL 1 = 3
turnL 2 = 4
turnL 3 = 2
turnL 4 = 1

displayDroid (posx', posy') status = do
  (posx, posy) <- gets pos
  g <- gets grid
  s' <- lift $ readArray g (posx, posy)
  liftIO $ setCursorPosition posy posx
  case s' of
    Just 1 -> liftIO $ putChar '·'
    Just 2 -> liftIO $ putChar '§'
    Just 3 -> liftIO $ putChar 'x'
  liftIO $ setCursorPosition posy' posx'
  case status of
    0 -> liftIO $ putChar '█' >> setCursorPosition posy posx >> putChar '®'
    _ -> liftIO $ putChar '®'
  liftIO $ hFlush stdout

updateAndDisplay :: Integer -> MazeState s
updateAndDisplay status = do
  g <- gets grid
  d <- gets mov
  p <- gets pos
  let p' = applyDir p d
  displayDroid p' status
  lift $ writeArray g p' (Just status)
  case status of
    0 -> return ()
    1 -> putPos p'
    2 -> putOxy (Just p') >> putPos p'

runProgram :: [Integer] -> InputF s -> STT s IO (ProgState s)
runProgram intprog input = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0 ..] intprog
  grid <- newArray ((-1024, -1024), (1024, 1024)) Nothing
  writeArray grid (60, 30) $ Just 3 -- Mark the initial spot
  let initState = ProgState 0 0 1 1 (60, 30) grid input Nothing
  -- evaluate the program
  execStateT (runProgram' prog) initState

runProgram' :: IntProg s -> MazeState s
runProgram' prog = do
  i <- gets ip
  instr <- lift $ readArray prog i
  case instr `rem` 100 of
    1  -> plusMult prog (+)
    2  -> plusMult prog (*)
    3  -> input prog
    4  -> output prog
    5  -> jump prog (/=)
    6  -> jump prog (==)
    7  -> cond prog (<)
    8  -> cond prog (==)
    9  -> adjBase prog
    99 -> return ()

shift offset instr = instr `div` place `rem` 10
  where
    place = 10 ^ (offset + 1)

getPmode prog offset = do
  i <- gets ip
  bp <- gets base
  lift $ shift offset <$> readArray prog i

{-# ANN readParam "Hlint: ignore Reduce duplication" #-}

readParam :: IntProg s -> Integer -> StateT (ProgState s) (STT s IO) Integer
readParam prog offset = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  lift $
    case pmode of
      0 -> readArray prog >=> readArray prog $ i + offset
      1 -> readArray prog (i + offset)
      2 -> readArray prog . (+ bp) =<< readArray prog (i + offset)

readParams :: IntProg s -> StateT (ProgState s) (STT s IO) (Integer, Integer)
readParams prog = liftM2 (,) (readParam prog 1) (readParam prog 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}

writeParam ::
     IntProg s -> Integer -> Integer -> StateT (ProgState s) (STT s IO) ()
writeParam prog offset val = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  c <-
    lift $
    case pmode of
      0 -> readArray prog (i + offset)
      2 -> (+ bp) <$> readArray prog (i + offset)
  void $ lift (writeArray prog c val)

putIP i = modify (\s -> s {ip = i})

putBase b = modify (\s -> s {base = b})

putPos p = modify (\s -> s {pos = p})

putMov m = modify (\s -> s {mov = m})

putDir m = modify (\s -> s {dir = m})

putOxy o = modify (\s -> s {oxy = o})

plusMult :: IntProg s -> IntOp -> MazeState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+ 4)
  runProgram' prog

getAi :: InputF s
getAi = do
  d <- gets dir
  m <- gets mov
  g <- gets grid
  p <- gets pos
  s <- lift $ readArray g (applyDir p m)
  case s of
    Just 0 -> newDir (turnR d) -- we found a wall
    _      -> putDir m >> newDir (turnR m) -- no wall, keep going

newDir :: Integer -> InputF s
newDir d = do
  p <- gets pos
  g <- gets grid
  let p' = applyDir p d
  s <- lift $ readArray g p'
  if s == Just 0
    then newDir (turnL d)
    else return d

input :: IntProg s -> MazeState s
input prog = do
  mov <- (join . gets) inputF
  writeParam prog 1 mov
  putMov mov
  gets ip >>= putIP . (+ 2)
  runProgram' prog

done :: StateT (ProgState s) (STT s IO) Bool
done = do
  g <- gets grid
  p <- gets pos
  d <- gets mov
  o <- gets oxy
  s' <- lift $ readArray g (applyDir p d)
  return $ s' == Just 3 && isJust o

output :: IntProg s -> MazeState s
output prog = do
  status <- readParam prog 1
  gets ip >>= putIP . (+ 2)
  end <- done
  updateAndDisplay status
  unless end $ runProgram' prog

jump :: IntProg s -> IntCond -> MazeState s
jump prog p = do
  i <- gets ip
  (a, b) <- readParams prog
  let i' =
        if p a 0
          then b
          else i + 3
  putIP i'
  runProgram' prog

cond :: IntProg s -> IntCond -> MazeState s
cond prog p = do
  (a, b) <- readParams prog
  let val =
        if p a b
          then 1
          else 0
  writeParam prog 3 val
  gets ip >>= putIP . (+ 4)
  runProgram' prog

adjBase :: IntProg s -> MazeState s
adjBase prog = do
  a <- readParam prog 1
  gets base >>= putBase . (+ a)
  gets ip >>= putIP . (+ 2)
  runProgram' prog
