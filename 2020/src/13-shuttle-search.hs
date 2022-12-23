import           Data.Function    ((&))
import           Data.List
import           Data.Tuple.Extra
import           Utils

sieve :: (Int, Int) -> (Int, Int) -> (Int, Int)
sieve (x, step) (r, q) =
  case find (\y -> y `mod` q == r) [x,x + step ..] of
    Nothing -> error "seive failed"
    Just x' -> (x', step * q)

main :: IO ()
main = do
  [read -> t, splitString (== ',') -> input] <- getFile "data/13-puzzle-input"
  let buses = zip [0 ..] input & filter ((/= "x") . snd) & map (second read)
      (t', id) = minimum $ map (\(_, id) -> (id - t `mod` id, id)) buses
      (p2, _) = foldl sieve (1, 1) $ map (\(min, id) -> ((id - min) `mod` id, id)) buses
  putStrLn $ "Part 1: " ++ show (t' `mod` t * id)
  putStrLn $ "Part 2: " ++ show p2
