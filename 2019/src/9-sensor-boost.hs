import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Array.ST
import Data.List
import Utils

-- prog, instruction pointer, relative base, inputs
data ProgState = ProgState
  { ip :: Integer,
    base :: Integer,
    inputs :: [Integer]
  }
  deriving (Show)

type IntProg s = STArray s Integer Integer

type StateWithOutputs s = StateT ProgState (ST s) [Integer]

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let inputs = splitString (== ',') input
      intprog = map read inputs
      p1 = runST $ runProgram intprog [1]
      p2 = runST $ runProgram intprog [2]
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

runProgram :: [Integer] -> [Integer] -> ST s [Integer]
runProgram intprog inputs = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0 ..] intprog
  let initState = ProgState 0 0 inputs
  -- evaluate the program
  evalStateT (runProgram' prog) initState

runProgram' :: IntProg s -> StateWithOutputs s
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
    99 -> lift $ return []

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

putInput xs = modify (\s -> s {inputs = xs})

putBase b = modify (\s -> s {base = b})

plusMult :: IntProg s -> IntOp -> StateWithOutputs s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+ 4)
  runProgram' prog

input :: IntProg s -> StateWithOutputs s
input prog = do
  (x : xs) <- gets inputs
  writeParam prog 1 x
  putInput xs
  gets ip >>= putIP . (+ 2)
  runProgram' prog

output :: IntProg s -> StateWithOutputs s
output prog = do
  a <- readParam prog 1
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
