module Main where

import qualified ChronalCalibration as CC
import qualified InventoryManagement as IM

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
      then return $ reverse lines
      else readLines (line:lines)


main :: IO ()
main = do
  input <- readLines []
  print $ CC.part1 input
