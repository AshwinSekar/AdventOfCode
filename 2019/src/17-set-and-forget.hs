import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.ST.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Data.Array.ST
import Data.Array
import Data.Char
import Data.List
import Data.Maybe

import Debug.Trace

import System.IO
import System.Console.ANSI
import System.Random

import Utils

-- prog, instruction pointer, relative base, inputs
data ProgState s = ProgState { ip :: Integer,
                               base :: Integer,
                               movement :: Integer,
                               key :: MVar Char,
                               inputF :: InputF s,
                               outputs :: [Int]
                             }
type IntProg s = STArray s Integer Integer
type GameState s = StateT (ProgState s) (STT s IO) ()
type IntOp = (Integer -> Integer -> Integer)
type IntCond = (Integer -> Integer -> Bool)
type InputF s = StateT (ProgState s) (STT s IO) Integer

liftIO :: IO a -> StateT (ProgState s) (STT s IO) a
liftIO = lift . lift

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/17-puzzle-input"
  k <- newEmptyMVar
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  runRobot intprog k
  showCursor


runRobot :: [Integer] -> MVar Char -> IO ()
runRobot prog k = do
  clearScreen
  hideCursor
  setupKeypress >> forkIO getKeypress
  hFlush stdout
  runSTT $ runProgram prog k getKey
  -- runSTT $ runProgram prog k getAi
  where getKeypress = getChar >>= putMVar k >> getKeypress
        setupKeypress = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

displayScaffold = do
  o <- gets outputs
  liftIO $ putStrLn (map chr o)

countIntersections = do
  o <- reverse <$> gets outputs
  let s = countIntersections' o
  liftIO $ print s
  liftIO $ hFlush stdout

countIntersections' out =
  let w = fromJust (elemIndex 10 out) + 1
      h = (length out `div` w)
      arr = listArray ((1,1), (h,w)) out :: Array (Int, Int) Int
      ints = [findInt arr (i, j) | i <- [1..h], j <- [1..w]]
  in sum $ catMaybes ints

findInt :: Array (Int, Int) Int -> (Int, Int) -> Maybe Int
findInt arr (i, j) =
  let dirs = [(-1, 0), (1, 0), (0, 1), (0, -1), (0, 0)]
      surround = map (sc arr (i, j)) dirs
      scaffolds = filter (== 35) surround
  in if length scaffolds == 5 then Just ((i - 1) * (j - 1)) else Nothing

sc arr (i, j) (y, x) =
  let (i', j') = (y + i, x + j)
      ((1, 1), (h, w)) = bounds arr
  in  if 1 <= i' && i' <= h && 1 <= j' && j' <= w
        then arr ! (i', j')
        else 0

runProgram :: [Integer] -> MVar Char -> InputF s -> STT s IO ()
runProgram intprog k input = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0..] intprog
  let initState = ProgState 0 0 0 k input []
  -- evaluate the program
  evalStateT (runProgram' prog >> displayScaffold >> countIntersections) initState


runProgram' :: IntProg s -> GameState s
runProgram' prog = do
  i <- gets ip
  instr <- lift $ readArray prog i
  case instr `rem` 100 of
    1 -> plusMult prog (+)
    2 -> plusMult prog (*)
    3 -> input prog
    4 -> output prog
    5 -> jump prog (/=)
    6 -> jump prog (==)
    7 -> cond prog (<)
    8 -> cond prog (==)
    9 -> adjBase prog
    99 -> return ()

shift offset instr = (instr `div` place) `rem` 10
  where place = 10 ^ (offset + 1)

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
  lift $ case pmode of
          0 -> readArray prog >=> readArray prog $ i + offset
          1 -> readArray prog (i + offset)
          2 -> readArray prog =<< ((+ bp) <$> readArray prog (i + offset))

readParams :: IntProg s -> StateT (ProgState s) (STT s IO) (Integer, Integer)
readParams prog = liftM2 (,) (readParam prog 1) (readParam prog 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}
writeParam :: IntProg s -> Integer -> Integer -> StateT (ProgState s) (STT s IO) ()
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
putMov m = modify (\s -> s {movement = m})
putOutputs o = modify (\s -> s {outputs = o})

plusMult :: IntProg s -> IntOp -> GameState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+4)
  runProgram' prog

getKey :: InputF s
getKey = do
  -- get input
  k <- wait
  case k of
    'w' -> return 1
    'a' -> return 3
    's' -> return 2
    'd' -> return 4
    _ -> getKey

getAi :: InputF s
getAi = liftIO $ randomRIO (1, 4)

input :: IntProg s -> GameState s
input prog = do
  mov <- (join . gets) inputF
  writeParam prog 1 mov
  putMov mov
  gets ip >>= putIP . (+2)
  runProgram' prog

wait =  do
  k <- (liftIO . tryTakeMVar) =<< gets key
  maybe ((liftIO . threadDelay) 5000 >> wait) return k

output :: IntProg s -> GameState s
output prog = do
  c <- fromIntegral <$> readParam prog 1
  gets outputs >>= putOutputs . (c:)
  gets ip >>= putIP . (+2)
  runProgram' prog

jump :: IntProg s -> IntCond -> GameState s
jump prog p = do
  i <- gets ip
  (a, b) <- readParams prog
  let i' = if p a 0 then b else i + 3
  putIP i'
  runProgram' prog

cond :: IntProg s -> IntCond -> GameState s
cond prog p = do
  (a, b) <- readParams prog
  let val = if p a b then 1 else 0
  writeParam prog 3 val
  gets ip >>= putIP . (+4)
  runProgram' prog

adjBase :: IntProg s -> GameState s
adjBase prog = do
  a <- readParam prog 1
  gets base >>= putBase . (+a)
  gets ip >>= putIP . (+2)
  runProgram' prog
