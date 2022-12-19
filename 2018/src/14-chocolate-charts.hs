import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Sequence               (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence               as Seq
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Tuple.Extra
import           Debug.Trace
import           Utils

type Plants = (Integer, [Bool])

type State = (Integer, Integer, [Bool])

main :: IO ()
main = do
  putStrLn "Input:"
  inp <- getLine
  let p1 = genSeq & drop (read inp) & take 10 <&> intToDigit
      Just p2 = genSeq & tails & findIndex (isPrefixOf (map digitToInt inp))
  putStrLn $ "Part 1: " ++ p1
  putStrLn $ "Part 2: " ++ show p2

genSeq :: [Int]
genSeq = 3 : 7 : gen 0 1 (3 :<| 7 :<| Empty)

gen :: Int -> Int -> Seq Int -> [Int]
gen i j seq =
  case (r `div` 10, r `rem` 10) of
    (0, x) -> x : gen (i' `rem` (n + 1)) (j' `rem` (n + 1)) (seq |> x)
    (y, x) -> y : x : gen (i' `rem` (n + 2)) (j' `rem` (n + 2)) (seq |> y |> x)
  where
    ri = Seq.index seq i
    rj = Seq.index seq j
    r = ri + rj
    i' = i + ri + 1
    j' = j + rj + 1
    n = Seq.length seq
