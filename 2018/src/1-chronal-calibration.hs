import qualified Data.Set as Set
import           Utils

parseStr :: String -> Integer
parseStr (y:ys)
  | y == '+' = read ys
  | otherwise = read (y : ys)

findDup :: [Integer] -> Set.Set Integer -> Integer
findDup (x:xs) s =
  if Set.member x s
    then x
    else findDup xs (Set.insert x s)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let p1 = part1 input
  let p2 = part2 input
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

part1 :: [String] -> Integer
part1 = sum . map parseStr

part2 :: [String] -> Integer
part2 input =
  let infInput = cycle $ map parseStr input
      prefSum = scanl (+) 0 infInput
   in findDup prefSum Set.empty
