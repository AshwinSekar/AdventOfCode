import           Utils

computeRecursiveFuel :: Int -> Int
computeRecursiveFuel fuel
  | fuel > 0 = fuel + computeRecursiveFuel (fuel `div` 3 - 2)
  | otherwise = 0

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let modules = map read input
  let p1_fuels = map (\x -> x `div` 3 - 2) modules
  let p2_fuels = map computeRecursiveFuel p1_fuels
  putStrLn $ "Part 1: " ++ show (sum p1_fuels)
  putStrLn $ "Part 2: " ++ show (sum p2_fuels)
