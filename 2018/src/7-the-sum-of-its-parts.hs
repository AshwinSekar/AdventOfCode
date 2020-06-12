import Utils

import Control.Monad
import Control.Monad.State.Strict

import Data.Foldable
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

import Debug.Trace

type AdjList = Map.Map Char (Set.Set Char)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let adj = parse input
  let p1 = topSort adj
  putStrLn $ "Part 1: " ++ p1
  -- putStrLn $ "Part 2: " ++ show p2


parse :: [String] -> AdjList
parse inputs =
  map edges inputs
    & (++ map sources inputs)
    & Map.fromListWith Set.union
  where edges s = (s !! 36, Set.singleton $ s !! 5)
        sources s = (s !! 5, Set.empty)

topSort :: AdjList -> String
topSort adj = reverse $ kahns adj []

kahns :: AdjList -> String -> String
kahns adj l =
  case Map.minViewWithKey sources of
    Nothing -> l
    Just ((u, _), _) -> kahns (del u) (u : l)
  where sources = Map.filter Set.null adj
        adj' = Map.deleteMin adj
        del u = Map.map (Set.delete u) $ Map.delete u adj
