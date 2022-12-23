import           Data.Array.Unboxed
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Utils

type Forest = UArray (Int, Int) Char

type Seen = Map.Map Forest Integer

main :: IO ()
main = do
  putStrLn "Input:"
  forest <- parse <$> readLines []
  putStrLn $ "Part 1: " ++ show (go forest 10)
  putStrLn $ "Part 2: " ++ show (go forest 1000000000)
  where
    go f n = step f Map.empty n 0

parse :: [String] -> Forest
parse s = listArray ((1, 1), (n, n)) $ concat s
  where
    n = length s

step :: Forest -> Seen -> Integer -> Integer -> Int
step f seen n i
  | n == i = length (filter (== '|') $ elems f) * length (filter (== '#') $ elems f)
  | otherwise =
    case (i -) <$> Map.lookup f seen of
      Nothing -> step f' (Map.insert f i seen) n (i + 1)
      Just c  -> step f' Map.empty (n `rem` c) ((i + 1) `rem` c)
  where
    f' = mapArrWithI (step' f) f

step' :: Forest -> (Int, Int) -> Char -> Char
step' f i c =
  case c of
    '.' ->
      if nTrees >= 3
        then '|'
        else '.'
    '#' ->
      if nTrees >= 1 && nLumber >= 1
        then '#'
        else '.'
    '|' ->
      if nLumber >= 3
        then '#'
        else '|'
  where
    ns = map (f !) $ filter (inRange $ bounds f) (neigh' i)
    nTrees = length $ filter (== '|') ns
    nLumber = length $ filter (== '#') ns
