import           Control.Applicative (liftA2)
import           Data.Function       ((&))
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Utils

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let expenses = map read input
      sums = liftA2 (,) expenses expenses & map (\p -> (uncurry (+) p, p)) & Map.fromList
      (x, y) = sums ! 2020
      s3:_ = filter (\e -> Map.member (2020 - e) sums) expenses
      (s1, s2) = sums ! (2020 - s3)
  putStrLn $ "Part 1: " ++ show (x * y)
  putStrLn $ "Part 2 " ++ show (s1 * s2 * s3)
