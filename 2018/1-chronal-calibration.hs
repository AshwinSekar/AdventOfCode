import qualified Data.Set as Set
import Debug.Trace

parseStr :: String -> Integer
parseStr (y:ys)
  | y == '+'  = read ys
  | otherwise = read (y:ys)

readLines :: [Integer] -> IO [Integer]
readLines xs = do
  line <- getLine
  if null line
      then return $ reverse xs
      else let x = parseStr line
           in readLines (x:xs)

findDup :: [Integer] -> Set.Set Integer -> Integer
findDup (x:xs) s = if Set.member x s
                      then x
                      else findDup xs (Set.insert x s)

part1 :: [Integer] -> Integer
part1 input = foldl (+) 0 input

part2 :: [Integer] -> Integer
part2 input =
  let infInput = cycle input
      prefSum = scanl (+) 0 infInput
  in  findDup prefSum Set.empty

main = do
  input <- readLines []
  print $ part2 input
