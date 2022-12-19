import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.List.Split
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Debug.Trace
import           Utils

type Plants = (Integer, [Bool])

type State = (Integer, Integer, [Bool])

main :: IO ()
main = do
  putStrLn "Input:"
  state <- readLines [] <&> head <&> splitOn ": " <&> (!! 1) <&> map (== '#')
  m <- parse <$> readLines []
  let p1 = genSum m state 20
      p2 = genSum m state 50000000000
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

parse :: [String] -> Set [Bool]
parse s =
  map (splitOn " => ") s & filter ((== "#") . (!! 1)) &
  map (map (== '#') . (!! 0)) &
  Set.fromList

genSum :: Set [Bool] -> [Bool] -> Int -> Int
genSum m state n =
  let (i, plants) = gen Map.empty 0 0 state
      gen st j i plants
        | j == n = (i, plants)
        | otherwise =
          case Map.lookup plants' st' of
            Nothing -> gen st' j' i' plants'
            Just (j0, i0) ->
              gen
                st'
                (n - ((n - j') `rem` (j' - j0)))
                (i' + (i' - i0) * ((n - j') `div` (j' - j0)))
                plants'
        where
          (length -> add, plants') =
            False : False : False : False : plants &
            (++ [False, False, False, False]) &
            divvy 5 1 &
            map (`Set.member` m) &
            dropWhileEnd not &
            span not
          (j', i') = (j + 1, i - 2 + add)
          st' = Map.insert plants (j, i) st
   in zip [i ..] plants & filter snd & map fst & sum
