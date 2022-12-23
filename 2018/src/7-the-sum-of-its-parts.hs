import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Map      as Map
import           Data.Maybe
import qualified Data.Set      as Set
import           Debug.Trace
import           Utils

-- Time, Step -> Steps before
type AdjList = Map.Map (Int, Char) (Set.Set Char)

-- (curTime, id)
type Worker = (Int, Int)

type Workers = Set.Set Worker

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let adj = parse input
      p1 = topSort adj
      p2 = parSort adj 5
  putStrLn $ "Part 1: " ++ p1
  putStrLn $ "Part 2: " ++ show p2

parse :: [String] -> AdjList
parse inputs = map edges inputs & (++ map sources inputs) & Map.fromListWith Set.union
  where
    edges s = ((0, s !! 36), Set.singleton $ s !! 5)
    sources s = ((0, s !! 5), Set.empty)

topSort :: AdjList -> String
topSort adj = reverse $ kahns adj []

kahns :: AdjList -> String -> String
kahns adj l =
  case Map.lookupMin sources of
    Nothing          -> l
    Just ((_, u), _) -> kahns (del u) (u : l)
  where
    sources = Map.filter Set.null adj
    del u = Map.map (Set.delete u) $ Map.delete (0, u) adj

parSort :: AdjList -> Int -> Int
parSort adj nWorkers = parKahns adj workers []
  where
    workers = Set.fromList $ map (0, ) [1 .. nWorkers]

parKahns :: AdjList -> Workers -> String -> Int
parKahns adj w l =
  case (Set.deleteFindMin w, Map.lookupMin sources) of
    (_, Nothing) -> fst $ Set.findMax w
    (((wt, wi), w'), Just ((t, u), _)) ->
      let t' = max wt t + time u
          w'' = Set.insert (t', wi) w'
       in parKahns (upd u t t' adj) w'' (u : l)
  where
    sources = Map.filter Set.null adj
    time u = ord u - ord 'A' + 61

upd :: Char -> Int -> Int -> AdjList -> AdjList
upd u t t' adj = Map.delete (t, u) adj & mapKWithV upd' & Map.map (Set.delete u)
  where
    upd' (ts, k) vs =
      if Set.member u vs
        then (max ts t', k)
        else (ts, k)
