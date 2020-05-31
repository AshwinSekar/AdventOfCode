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
                               curPos :: Maybe (Int, Int),
                               grid :: STArray s (Int, Int) Int
                             }
type IntProg s = STArray s Integer Integer
type GameState s = StateT (ProgState s) (STT s IO) ()
type IntOp = (Integer -> Integer -> Integer)
type IntCond = (Integer -> Integer -> Bool)

liftIO :: IO a -> StateT (ProgState s) (STT s IO) a
liftIO = lift . lift

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/19-puzzle-input"
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  p1 <- countTractor intprog 49
  putStrLn $ "Part 1: " ++ show p1

countTractor :: [Integer] -> Int -> IO Int
countTractor prog b = runSTT $ count =<< findGrid prog b

count :: STArray s (Int, Int) Int -> STT s IO Int
count g = length <$> (filterM (return . (== 1)) =<< getElems g)

findGrid :: [Integer] -> Int -> STT s IO (STArray s (Int, Int) Int)
findGrid prog b =
  let pos = concat $ liftM2 lister [0..b] [0..b]
  in runProgram prog pos
  where lister a b = [a, b]

displayGrid :: STArray s (Int, Int) Int -> Int -> STT s IO ()
displayGrid g b = do
  elems <- getElems g
  let chars = map disp elems
      gs = slices b chars
  lift $ putStrLn (unlines gs)
  where disp 1 = '#'
        disp 0 = '.'

runProgram :: [Integer] -> [Int] -> STT s IO (STArray s (Int, Int) Int)
runProgram intprog pos = do
  let bound = floor $ sqrt (fromIntegral (length pos) / 2)
  g <- newArray ((0, 0), (bound - 1, bound - 1)) (-1)
  let initState = ProgState 0 0 pos Nothing g
  -- evaluate the program
  g <- grid <$> execStateT (drain intprog) initState
  -- display
  displayGrid g bound
  return g

drain :: [Integer] -> GameState s
drain intprog = do
  prog <- lift $ newArray (0, 4096) 0
  lift $ zipWithM_ (writeArray prog) [0..] intprog
  ins <- gets inputs
  if null ins
    then return ()
    else putIP 0  >> putBase 0 >> runProgram' prog >> drain intprog

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
putPos p = modify (\s -> s {curPos = p})
putInputs i = modify (\s -> s {inputs = i})

plusMult :: IntProg s -> IntOp -> GameState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+4)
  runProgram' prog

getPos Nothing (y:x:_) = return $ Just (y, x)
getPos pos _ = return pos

input :: IntProg s -> GameState s
input prog = do
  cur <- gets curPos
  m:ins <- gets inputs
  putPos =<< getPos cur =<< gets inputs
  writeParam prog 1 (fromIntegral m)
  putInputs ins
  gets ip >>= putIP . (+2)
  runProgram' prog

output :: IntProg s -> GameState s
output prog = do
  c <- fromIntegral <$> readParam prog 1
  Just (y, x) <- gets curPos
  g <- gets grid
  lift $ writeArray g (y, x) c
  putPos Nothing
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
