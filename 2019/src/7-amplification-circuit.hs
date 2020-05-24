import Control.Applicative
import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.List

import Utils

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let inputs = splitString (== ',') input
      intprog = map read inputs
      phases = permutations [0,1,2,3,4]
      maxThrust = maximum $ map (findThrust intprog) phases
  putStrLn $ "Part 1: " ++ show maxThrust
  -- putStrLn $ "Part 2: " ++ show p2


findThrust :: [Int] -> [Int] -> Int
findThrust intprog = foldl' (thrust intprog) 0

thrust :: [Int] -> Int -> Int -> Int
thrust intprog out phase =
  let [signal] = runST $ runProgram intprog [phase, out]
  in signal

runProgram :: [Int] -> [Int] -> ST s [Int]
runProgram intprog inputs = do
  prog <- newListArray (0, length intprog) intprog :: ST s (STArray s Int Int)
  -- evaluate the program
  runProgram' prog inputs 0


runProgram' :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
runProgram' prog inputs i = do
  instr <- readArray prog i
  case instr `rem` 100 of
    1 -> plusMult prog inputs i
    2 -> plusMult prog inputs i
    3 -> input prog inputs i
    4 -> output prog inputs i
    5 -> jump prog inputs i
    6 -> jump prog inputs i
    7 -> cond prog inputs i (<)
    8 -> cond prog inputs i (==)
    99 -> return []

getPmodes instr =
  let pmodes = instr `div` 100
  in (pmodes `rem` 10, (pmodes `div` 10) `rem` 10)

getParams prog i = do
  instr <- readArray prog i
  let (pa, pb) = getPmodes instr
  a <- readArray prog (i + 1)
  b <- readArray prog (i + 2)
  a <- if pa == 0 then readArray prog a else return a
  b <- if pb == 0 then readArray prog b else return b
  return (a,b)

plusMult :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
plusMult prog inputs i = do
  -- find parameter mode
  opcode <- (`rem` 100) <$> readArray prog i
  (a, b) <- getParams prog i
  c <- readArray prog (i + 3)
  let op = case opcode of
            1 -> (+)
            2 -> (*)
  writeArray prog c $ a `op` b
  runProgram' prog inputs (i + 4)

input :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
input prog (x:xs) i = do
  a <- readArray prog (i + 1)
  writeArray prog a x
  runProgram' prog xs (i + 2)

output :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
output prog inputs i = do
  -- find parameter mode
  (pa, _) <- getPmodes <$> readArray prog i
  a <- readArray prog (i + 1)
  a <- if pa == 0 then readArray prog a else return a
  (a:) <$> runProgram' prog inputs (i + 2)

jump :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
jump prog inputs i = do
  opcode <- (`rem` 100) <$> readArray prog i
  (a, b) <- getParams prog i
  let cond = case opcode of
              5 -> a /= 0
              6 -> a == 0
  if cond then runProgram' prog inputs b else runProgram' prog inputs (i + 3)

cond :: STArray s Int Int -> [Int] -> Int -> (Int -> Int -> Bool) -> ST s [Int]
cond prog inputs i p = do
  (a, b) <- getParams prog i
  let val = if p a b then 1 else 0
  c <- readArray prog (i + 3)
  writeArray prog c val
  runProgram' prog inputs (i + 4)
