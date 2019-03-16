module Main where

import qualified ChronalCalibration as CC
import qualified InventoryManagement as IM
import System.IO (hFlush, stdout)

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
      then return $ reverse lines
      else readLines (line:lines)

part1 :: Int -> [String] -> IO ()
part1 1 = print . CC.part1
part1 2 = print . IM.part1

part2 :: Int -> [String] -> IO ()
part2 1 = print . CC.part2
part2 2 = print .IM.part2

main :: IO ()
main = do
  putStr "Day?: "
  hFlush stdout
  day <- getLine
  input <- readLines []
  putStrLn "----- Part 1: -----"
  part1 (read day) input
  putStrLn ""
  putStrLn "----- Part 2: -----"
  part2 (read day) input
