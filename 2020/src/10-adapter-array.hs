import           Data.Function ((&))
import           Data.List
import           Data.Map      ((!))
import qualified Data.Map      as Map
import           Data.Maybe
import           Utils

main :: IO ()
main = do
  adapt <- sort . map read <$> getFile "data/10-puzzle-input"
  let max = 3 + maximum adapt
      diffs = zipWith (-) (adapt ++ [max]) (0 : adapt)
      dpChain i
        | i == max = (i, 1)
        | i `notElem` (0 : adapt) = (i, 0)
        | otherwise =
          (i, [i + 1, i + 2, i + 3] & mapMaybe (`Map.lookup` memo) & sum)
      memo = Map.fromList $ map dpChain (0 : adapt ++ [max])
  putStrLn $ "Part 1: " ++ show (count (== 1) diffs * count (== 3) diffs)
  putStrLn $ "Part 2: " ++ show (memo ! 0)
