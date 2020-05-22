import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import Utils
import Debug.Trace

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let inputs = splitString (== ',') input
      p1 = runST $ part1 $ map read inputs
  putStrLn $ "Part 1: " ++ show p1

part1 :: [Int] -> ST s Int
part1 input = do
  prog <- newListArray (0, length input) input :: ST s (STArray s Int Int)
  -- 1202 alarm modifications
  writeArray prog 1 12
  writeArray prog 2 2
  -- evaluate the program
  runProgram prog 0
  readArray prog 0

runProgram :: STArray s Int Int -> Int -> ST s ()
runProgram prog i = do
  opcode <- readArray prog i
  unless (opcode == 99) $ do
    p1 <- readArray prog (i + 1)
    p2 <- readArray prog (i + 2)
    p3 <- readArray prog (i + 3)
    let op = case opcode of
              1 -> (+)
              2 -> (*)
    a <- readArray prog p1
    b <- readArray prog p2
    writeArray prog p3 $ a `op` b
    runProgram prog (i + 4)
