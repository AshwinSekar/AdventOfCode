import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List
import           Utils

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let inputs = splitString (== ',') input
      intprog = map read inputs
      p1 = runST $ runProgram intprog (12, 2)
      results =
        map (\x -> (x, runST $ runProgram intprog x)) $
        liftA2 (,) [0 .. 99] [0 .. 99]
      Just ((noun, verb), _) = find ((== 19690720) . snd) results
      p2 = 100 * noun + verb
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

runProgram :: [Int] -> (Int, Int) -> ST s Int
runProgram input (noun, verb) = do
  prog <- newListArray (0, length input) input :: ST s (STArray s Int Int)
  -- noun verb modifications
  writeArray prog 1 noun
  writeArray prog 2 verb
  -- evaluate the program
  runProgram' prog 0
  readArray prog 0

runProgram' :: STArray s Int Int -> Int -> ST s ()
runProgram' prog i = do
  opcode <- readArray prog i
  unless (opcode == 99) $ do
    a <- readArray prog =<< readArray prog (i + 1)
    b <- readArray prog =<< readArray prog (i + 2)
    p3 <- readArray prog (i + 3)
    let op =
          case opcode of
            1 -> (+)
            2 -> (*)
    writeArray prog p3 $ a `op` b
    runProgram' prog (i + 4)
