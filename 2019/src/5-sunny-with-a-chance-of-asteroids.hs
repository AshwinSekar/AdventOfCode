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
      p1 = runST $ runProgram intprog
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2


runProgram :: [Int] -> ST s [Int]
runProgram input = do
  prog <- newListArray (0, length input) input :: ST s (STArray s Int Int)
  -- evaluate the program
  runProgram' prog 0


runProgram' :: STArray s Int Int -> Int -> ST s [Int]
runProgram' prog i = do
  instr <- readArray prog i
  case instr `rem` 100 of
    1 -> plusMult prog i
    2 -> plusMult prog i
    3 -> input prog i
    4 -> output prog i
    5 -> jump prog i
    6 -> jump prog i
    7 -> cond prog i (<)
    8 -> cond prog i (==)
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

plusMult :: STArray s Int Int -> Int -> ST s [Int]
plusMult prog i = do
  -- find parameter mode
  opcode <- (`rem` 100) <$> readArray prog i
  (a, b) <- getParams prog i
  c <- readArray prog (i + 3)
  let op = case opcode of
            1 -> (+)
            2 -> (*)
  writeArray prog c $ a `op` b
  runProgram' prog (i + 4)

input :: STArray s Int Int -> Int -> ST s [Int]
input prog i = do
  a <- readArray prog (i + 1)
  writeArray prog a 5
  runProgram' prog (i + 2)

output :: STArray s Int Int -> Int -> ST s [Int]
output prog i = do
  -- find parameter mode
  (pa, _) <- getPmodes <$> readArray prog i
  a <- readArray prog (i + 1)
  a <- if pa == 0 then readArray prog a else return a
  (a:) <$> runProgram' prog (i + 2)

jump :: STArray s Int Int -> Int -> ST s [Int]
jump prog i = do
  opcode <- (`rem` 100) <$> readArray prog i
  (a, b) <- getParams prog i
  let cond = case opcode of
              5 -> a /= 0
              6 -> a == 0
  if cond then runProgram' prog b else runProgram' prog (i + 3)

cond :: STArray s Int Int -> Int -> (Int -> Int -> Bool) -> ST s [Int]
cond prog i p = do
  (a, b) <- getParams prog i
  let val = if p a b then 1 else 0
  c <- readArray prog (i + 3)
  writeArray prog c val
  runProgram' prog (i + 4)
