import Control.Applicative (liftA2)
import Data.Function ((&))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils

main :: IO ()
main = do
  input <- getFile "data/1-puzzle-input"
  let depths = map read input
      n = length depths
      f =
        foldl
          ( \(prev, s) n ->
              ( n,
                if n > prev
                  then s + 1
                  else s
              )
          )
          (maxBound :: Int, 0)
      (_, p1) = f depths
      windows =
        zipWith3
          (\a b c -> a + b + c)
          (take (n - 2) depths)
          (take (n - 1) (drop 1 depths))
          (drop 2 depths)
      (_, p2) = f windows
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2 " ++ show p2
