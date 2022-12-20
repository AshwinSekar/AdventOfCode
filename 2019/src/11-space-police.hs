import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Array.ST
import           Data.Bool
import           Data.List
import qualified Data.Set                       as Set
import           Debug.Trace
import           Utils

-- prog, instruction pointer, relative base, inputs
data ProgState = ProgState
  { ip      :: Integer,
    base    :: Integer,
    pos     :: (Integer, Integer),
    dir     :: (Integer, Integer),
    toPaint :: Bool,
    white   :: Set.Set (Integer, Integer),
    cnt     :: Set.Set (Integer, Integer)
  }
  deriving (Show)

type IntProg s = STArray s Integer Integer

type StateWithOutputs s = StateT ProgState (ST s) [Integer]

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

-- Brain state
shift offset instr = (instr `div` place) `rem` 10
  where
    place = 10 ^ (offset + 1)

getPmode prog offset = do
  i <- gets ip
  bp <- gets base
  lift $ shift offset <$> readArray prog i

{-# ANN readParam "Hlint: ignore Reduce duplication" #-}
readParam :: IntProg s -> Integer -> StateT ProgState (ST s) Integer
readParam prog offset = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  lift $ case pmode of
    0 -> readArray prog >=> readArray prog $ i + offset
    1 -> readArray prog (i + offset)
    2 -> readArray prog =<< ((+ bp) <$> readArray prog (i + offset))

readParams :: IntProg s -> StateT ProgState (ST s) (Integer, Integer)
readParams prog = liftM2 (,) (readParam prog 1) (readParam prog 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}
writeParam :: IntProg s -> Integer -> Integer -> StateT ProgState (ST s) ()
writeParam prog offset val = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  c <- lift $ case pmode of
    0 -> readArray prog (i + offset)
    2 -> (+ bp) <$> readArray prog (i + offset)
  void $ lift (writeArray prog c val)

putIP i = modify (\s -> s {ip = i})

putBase b = modify (\s -> s {base = b})

-- Robot state
move :: StateT ProgState (ST s) ()
move = liftM2 (\(x, y) (x', y') -> (x + x', y + y')) (gets pos) (gets dir) >>= putPos

turn :: Integer -> StateT ProgState (ST s) ()
turn a = gets dir >>= putDir . turnDir a

paint :: Integer -> StateT ProgState (ST s) ()
paint 1 = liftM2 Set.insert (gets pos) (gets white) >>= putWhite
paint 0 = liftM2 Set.delete (gets pos) (gets white) >>= putWhite

updateCnt :: StateT ProgState (ST s) ()
updateCnt = liftM2 Set.insert (gets pos) (gets cnt) >>= putCnt

turnDir 0 (0, 1)  = (-1, 0)
turnDir 0 (-1, 0) = (0, -1)
turnDir 0 (0, -1) = (1, 0)
turnDir 0 (1, 0)  = (0, 1)
turnDir 1 (-1, 0) = (0, 1)
turnDir 1 (0, -1) = (-1, 0)
turnDir 1 (1, 0)  = (0, -1)
turnDir 1 (0, 1)  = (1, 0)

putWhite p = modify (\s -> s {white = p})

putCnt p = modify (\s -> s {cnt = p})

putToPaint p = modify (\s -> s {toPaint = p})

putPos p = modify (\s -> s {pos = p})

putDir p = modify (\s -> s {dir = p})

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let inputs = splitString (== ',') input
      brain = map read inputs
      p1State = ProgState 0 0 (0, 0) (0, 1) True Set.empty Set.empty
      p1 = runST $ cnt <$> driveRobot brain p1State
      p2State = ProgState 0 0 (0, 0) (0, 1) True (Set.singleton (0, 0)) Set.empty
      p2 = ascii $ runST (white <$> driveRobot brain p2State)
  putStrLn $ "Part 1: " ++ show (Set.size p1)
  putStrLn $ "Part 2: \n" ++ unlines p2

ascii whites =
  let (xs, xb) = (Set.findMin $ Set.map fst whites, Set.findMax $ Set.map fst whites)
      (ys, yb) = (Set.findMin $ Set.map snd whites, Set.findMax $ Set.map snd whites)
   in map (\y -> map (asciiChar y) [xs .. xb]) $ reverse [ys .. yb]
  where
    asciiChar y x = if Set.member (x, y) whites then '#' else ' '

driveRobot :: [Integer] -> ProgState -> ST s ProgState
driveRobot brain initState = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0 ..] brain
  let -- evaluate the program

  execStateT (runProgram' prog) initState

runProgram' :: IntProg s -> StateWithOutputs s
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
    99 -> lift $ return []

plusMult :: IntProg s -> IntOp -> StateWithOutputs s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+ 4)
  runProgram' prog

input :: IntProg s -> StateWithOutputs s
input prog = do
  c <- bool 0 1 <$> liftM2 Set.member (gets pos) (gets white)
  writeParam prog 1 c
  gets ip >>= putIP . (+ 2)
  runProgram' prog

output :: IntProg s -> StateWithOutputs s
output prog = do
  a <- readParam prog 1
  toPaint <- gets toPaint
  if toPaint then paint a >> updateCnt else turn a >> move
  putToPaint $ not toPaint
  gets ip >>= putIP . (+ 2)
  (a :) <$> runProgram' prog

jump :: IntProg s -> IntCond -> StateWithOutputs s
jump prog p = do
  i <- gets ip
  (a, b) <- readParams prog
  let i' = if p a 0 then b else i + 3
  putIP i'
  runProgram' prog

cond :: IntProg s -> IntCond -> StateWithOutputs s
cond prog p = do
  (a, b) <- readParams prog
  let val = if p a b then 1 else 0
  writeParam prog 3 val
  gets ip >>= putIP . (+ 4)
  runProgram' prog

adjBase :: IntProg s -> StateWithOutputs s
adjBase prog = do
  a <- readParam prog 1
  gets base >>= putBase . (+ a)
  gets ip >>= putIP . (+ 2)
  runProgram' prog
