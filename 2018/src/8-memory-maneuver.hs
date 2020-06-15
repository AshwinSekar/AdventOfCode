import Utils

import Control.Monad

import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe

import Debug.Trace

data Node = Node [Node] [Int] deriving (Show)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- map read . splitString (== ' ') <$> readFile "data/8-puzzle-input"
  let root = parse input
      p1 = sumMetadata input
      p2 = value root
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2


sumMetadata :: [Int] -> Integer
sumMetadata nodes = s
  where ([], s) = sumMetadata' (nodes, 0)

sumMetadata' :: ([Int], Integer) -> ([Int], Integer)
sumMetadata' ([], s) = ([], s)
sumMetadata' (n:m:ns, s) = (ns'', s'')
  where (ns', s') = iterate sumMetadata' (ns, s) & (!! n)
        (meta, ns'') = splitAt m ns'
        s'' = sum (map fromIntegral meta) + s'


parse :: [Int] -> Node
parse ins = head . fst $ parse' ([], ins)

parse' :: ([Node], [Int]) -> ([Node], [Int])
parse' (ns, n:m:is) = (s:ns, is'')
  where (c, is') = iterate parse' ([], is) & (!! n)
        (meta, is'') = splitAt m is'
        s = Node c meta

value :: Node -> Integer
value (Node [] m) = sum (map fromIntegral m)
value (Node c m) = map (mlen -) m
                    & filter (>= 0)
                    & map (c !!)
                    & map value
                    & sum
  where mlen = length c
