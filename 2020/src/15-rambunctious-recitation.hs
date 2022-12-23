import qualified Data.IntMap        as Map
import           Data.IntMap.Strict ((!))
import           Text.Megaparsec
import           Utils

play :: Int -> Map.IntMap Int -> Int -> Int -> Int
play turns mem last i
  | i == turns + 1 = last
  | Map.member last mem = play turns mem' (i - 1 - mem ! last) (i + 1)
  | otherwise = play turns mem' 0 (i + 1)
  where
    mem' = Map.insert last (i - 1) mem

main :: IO ()
main = do
  input <- parseFile "data/15-puzzle-input" (decimal `sepBy` symbol ",")
  let start = Map.fromList $ zip input [1 ..]
  putStrLn $ "Part 1: " ++ show (play 2020 start (last input) (length input + 1))
  putStrLn $ "Part 2: " ++ show (play 30000000 start (last input) (length input + 1))
