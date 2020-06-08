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
                               inputs :: [Int],
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
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  path <- runRobot intprog False
  let path' = group path
      path'' = concatMap compress path'
  showCursor
  putStrLn path
  putStrLn path''
  void $ runRobot intprog True

compress s = case head s of
  'x' -> show (length s) ++ "-"
  a -> a : "-"

runRobot :: [Integer] -> Bool -> IO String
runRobot prog t = do
  clearScreen
  hFlush stdout
  runSTT $ runProgram prog t

countIntersections = do
  o <- reverse <$> gets outputs
  let s = countIntersections' o
  liftIO $ putStrLn ("Part 1: " ++ show s)
  liftIO $ hFlush stdout

countIntersections' out =
  let w = fromJust (elemIndex 10 out) + 1
      h = length out `div` w
      arr = listArray ((1,1), (h,w)) out
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

path :: StateT (ProgState s) (STT s IO) String
path = do
  o <- reverse <$> gets outputs
  let w = fromJust (elemIndex 10 o) + 1
      h = length o `div` w
      arr = listArray ((1,1), (h,w)) o
      pos = (1, 33)
      dir = (-1, 0)
  return $ path' arr pos dir

path' arr pos dir =
  let r = turnR dir
      l = turnL dir
  in case (sc arr pos l, sc arr pos dir, sc arr pos r) of
       (_, 35, _) -> 'x' : path' arr (pos `tplus` dir) dir
       (35, _, _) -> 'l' : path' arr pos l
       (_, _, 35) -> 'r' : path' arr pos r
       _ -> []

(y, x) `tplus` (y', x') = (y + y', x + x')

turnR (-1, 0) = (0, 1)
turnR (0, 1)  = (1, 0)
turnR (1, 0)  = (0, -1)
turnR (0, -1) = (-1, 0)

turnL (0, 1)  = (-1, 0)
turnL (1, 0)  = (0, 1)
turnL (0, -1) = (1, 0)
turnL (-1, 0) = (0, -1)

runProgram :: [Integer] -> Bool -> STT s IO String
runProgram intprog t = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0..] intprog
  when t $ writeArray prog 0 2
  let initState = ProgState 0 0 [] []
  -- evaluate the program
  evalStateT (runProgram' prog >> countIntersections >> path) initState


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
putInputs i = modify (\s -> s {inputs = i})
putOutputs o = modify (\s -> s {outputs = o})

plusMult :: IntProg s -> IntOp -> GameState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+4)
  runProgram' prog

getInput :: [Int] -> StateT (ProgState s) (STT s IO) [Int]
getInput (x:xs) = return $ x:xs
getInput [] = do
  inputs <- liftIO $ readLines []
  let withNLine = map (\s -> s ++ [chr 10]) inputs
      ascii = concatMap (map ord) withNLine
  putInputs ascii
  return ascii

input :: IntProg s -> GameState s
input prog = do
  m:ins <- getInput =<< gets inputs
  writeParam prog 1 (fromIntegral m)
  putInputs ins
  gets ip >>= putIP . (+2)
  runProgram' prog

output :: IntProg s -> GameState s
output prog = do
  c <- fromIntegral <$> readParam prog 1
  if c <= 128
    then liftIO $ putChar (chr c)
    else liftIO $ putStrLn ("Total dust: " ++ show c)
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
