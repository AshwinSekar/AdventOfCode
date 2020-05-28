import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Data.Array.ST
import Data.List
import Debug.Trace


import Utils


-- progs, instruction pointers, inputs, phase, currentAmp
data ProgState s = ProgState { ips :: STArray s Int Integer,
                               amps :: [IntProg s],
                               currentAmp :: Int,
                               inputs :: [Integer],
                               phase :: [Integer]
                             }
type IntProg s = STArray s Integer Integer
type StateWithOutputs s = StateT (ProgState s) (ST s) [Integer]
type IntOp = (Integer -> Integer -> Integer)
type IntCond = (Integer -> Integer -> Bool)


main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let inputs = splitString (== ',') input
      prog = map read inputs
      p1Phases = permutations [0..4]
      maxThrust = maximum $ map (findThrust prog) p1Phases
  putStrLn $ "Part 1: " ++ show maxThrust
  -- putStrLn $ "Part 2: " ++ show p2

findThrust :: [Integer] -> [Integer] -> Integer
findThrust prog phase = head $ runST (runProgram prog phase)


-- intprog state
shift offset instr = (instr `div` place) `rem` 10
  where place = 10 ^ (offset + 1)

getPmode offset = do
  i <- getIP
  prog <- getProg
  lift $ shift offset <$> readArray prog i

{-# ANN readParam "Hlint: ignore Reduce duplication" #-}
readParam :: Integer -> StateT (ProgState s) (ST s) Integer
readParam offset = do
  i <- getIP
  prog <- getProg
  pmode <- getPmode offset
  lift $ case pmode of
          0 -> readArray prog >=> readArray prog $ i + offset
          1 -> readArray prog (i + offset)

readParams :: StateT (ProgState s) (ST s) (Integer, Integer)
readParams = liftM2 (,) (readParam 1) (readParam 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}
writeParam :: Integer -> Integer -> StateT (ProgState s) (ST s) ()
writeParam offset val = do
  prog <- getProg
  i <- (+ offset) <$> getIP
  c <- lift $ readArray prog i
  void $ lift (writeArray prog c val)

getIP :: StateT (ProgState s) (ST s) Integer
getIP = do is <- gets ips
           c <- gets currentAmp
           lift $ readArray is c

getProg :: StateT (ProgState s) (ST s) (STArray s Integer Integer)
getProg = liftM2 (!!) (gets amps) (gets currentAmp)

incIP x = ((+ x) <$> getIP) >>= putIP

putIP :: Integer -> StateT (ProgState s) (ST s) ()
putIP i = do
  is <- gets ips
  c <- gets currentAmp
  lift $ writeArray is c i

putInputs xs = modify (\s -> s {inputs = xs})
putCurAmp c = modify (\s -> s {currentAmp = c})

runProgram :: [Integer] -> [Integer] -> ST s [Integer]
runProgram intprog phase = do
  progs <- mapM (\_ -> newArray (0, 4096) 0) [0..4]
  mapM_ (populate intprog) progs
  is <- newArray (0, 5) 0
  -- evaluate the program
  evalStateT runProgram' $ ProgState is progs 0 inputs phase
  where inputs = [head phase, 0]

populate :: [Integer] -> STArray s Integer Integer -> ST s [()]
populate intprog prog = zipWithM (writeArray prog) [0..] intprog


runProgram' :: StateWithOutputs s
runProgram' = do
  prog <- getProg
  ip <- getIP
  instr <- lift $ readArray prog ip
  case instr `rem` 100 of
    1 -> plusMult (+)
    2 -> plusMult (*)
    3 -> input
    4 -> output
    5 -> jump (/=)
    6 -> jump (==)
    7 -> cond (<)
    8 -> cond (==)
    99 -> lift $ return []
    _ -> fail ("Unexpected instr: " ++ show instr)

plusMult :: IntOp -> StateWithOutputs s
plusMult op = do
  (a, b) <- readParams
  writeParam 3 $ a `op` b
  incIP 4
  runProgram'

input :: StateWithOutputs s
input = do
  (x:xs) <- gets inputs
  writeParam 1 x
  putInputs xs
  incIP 2
  runProgram'

output :: StateWithOutputs s
output = do
  a <- readParam 1
  nextAmp <- (+1) <$> gets currentAmp
  incIP 2
  if nextAmp > 4 then (a:) <$> runProgram' else do
    nextPhase <- (!! nextAmp) <$> gets phase
    putCurAmp nextAmp
    putInputs [nextPhase, a]
    runProgram'

jump :: IntCond -> StateWithOutputs s
jump p = do
  i <- getIP
  (a, b) <- readParams
  let i' = if p a 0 then b else i + 3
  putIP i'
  runProgram'

cond :: IntCond -> StateWithOutputs s
cond p = do
  (a, b) <- readParams
  let val = if p a b then 1 else 0
  writeParam 3 val
  incIP 4
  runProgram'
