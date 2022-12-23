import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set
import           Utils

type Seats = Map.Map (Integer, Integer) Char

findStable :: (Seats -> (Integer, Integer) -> Char -> Char) -> Set.Set Seats -> Seats -> Int
findStable step seen seats
  | Set.member seats seen = count (== '#') $ Map.elems seats
  | otherwise = findStable step seen' seats'
  where
    seats' = Map.mapWithKey (step seats) seats
    seen' = Set.insert seats seen

seatFlip adj occN seats pos 'L'
  | count (== '#') (adj seats pos) == 0 = '#'
seatFlip adj occN seats pos '#'
  | count (== '#') (adj seats pos) >= occN = 'L'
seatFlip adj occN seats pos c = c

imm8Adj seats pos = mapMaybe ((`Map.lookup` seats) . psum pos) eightDirs

ray8Adj seats pos = mapMaybe (look pos) eightDirs
  where
    look pos dir =
      case Map.lookup (psum pos dir) seats of
        Nothing    -> Nothing
        (Just '.') -> look (psum pos dir) dir
        (Just c)   -> Just c

main :: IO ()
main = do
  seats <- gridMap <$> getFile "data/11-puzzle-input"
  putStrLn $ "Part 1: " ++ show (findStable (seatFlip imm8Adj 4) Set.empty seats)
  putStrLn $ "Part 2: " ++ show (findStable (seatFlip ray8Adj 5) Set.empty seats)
