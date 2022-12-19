import           Control.Applicative (empty, liftA2, (<|>))
import           Data.Char           (digitToInt)
import           Data.Function       ((&))
import           Data.Map            ((!))
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import qualified Data.Set            as Set
import           Utils

type OctoMap = Map.Map (Integer, Integer) Int

main :: IO ()
main = do
  octopi <- gridMap <$> getFile "data/11-puzzle-input"
  let init = Map.map digitToInt octopi
      (_, p1) = iterate step (init, 0) !! 100
      p2 = sync 0 init
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

step :: (OctoMap, Int) -> (OctoMap, Int)
step (o, f) = (o', Map.size (Map.filter (== 0) o') + f)
  where
    o' = flashAll $ Map.map (+ 1) o

sync s o =
  if Map.null $ Map.filter (> 0) o
    then s
    else sync (s + 1) (fst $ step (o, 0))

flashAll :: OctoMap -> OctoMap
flashAll octopi =
  if Set.null (readyToFlash octopi)
    then octopi
    else flashAll octopi'
  where
    readyToFlash o = Map.keysSet $ Map.filter (> 9) o
    octopi' =
      Set.foldl
        (\o p -> foldl inc (flash p o) (neighs p o))
        octopi
        (readyToFlash octopi)
    flash p = Map.insert p 0
    inc = flip $ Map.adjust (+ 1)
    neighs p o = filter (isJust . valid o) $ map (psum p) eightDirs

valid o p = do
  c <- o Map.!? p
  if c > 0
    then pure c
    else empty
