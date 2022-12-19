import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Map         as Map
import           Data.Maybe
import qualified Data.Set         as Set
import           Data.Tuple.Extra
import           Debug.Trace
import           Utils

-- Guard # -> [(sleep, wake)]
type GuardIntervals = Map.Map Int [(Int, Int)]

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let ins = parse input
      p1 = checkSum ins $ strat1Guard ins
      p2 = checkSum ins $ strat2Guard ins
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

parse :: [String] -> GuardIntervals
parse inputs = sort inputs & foldl parseInt (Map.empty, 0, 0) & fst3

parseInt :: (GuardIntervals, Int, Int) -> String -> (GuardIntervals, Int, Int)
parseInt (ints, g, sl) s =
  case (s !! 25, readDigits 15, readDigits 26) of
    ('#', _, g)  -> (ints, g, sl)
    ('a', sl, _) -> (ints, g, sl)
    ('u', n, _)  -> (Map.insertWith (++) g [(sl, n)] ints, g, sl)
  where
    readDigits n = s & read . takeWhile isDigit . drop n

freqMin :: [(Int, Int)] -> (Int, Int)
freqMin ints =
  concatMap splitTuple ints & sort & scanl accumInt (0, 0) & maximum
  where
    splitTuple (b, e) = [(b, 1), (e, -1)]
    accumInt (cnt, _) (g, sign) = (cnt + sign, g)

strat1Guard :: GuardIntervals -> Int
strat1Guard ints = kForMaxV $ Map.map (sum . map (\(s, e) -> e - s)) ints

strat2Guard :: GuardIntervals -> Int
strat2Guard ints = kForMaxV $ Map.map freqMin ints

checkSum :: GuardIntervals -> Int -> Int
checkSum ints guard = guard * (snd . freqMin $ ints Map.! guard)
