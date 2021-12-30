import           Utils

import           Control.Applicative (liftA2, (<|>))

import           Data.Char           (digitToInt)
import           Data.Function       ((&))
import           Data.List           (sort)
import           Data.Map            ((!), (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, mapMaybe)
import qualified Data.Set            as Set

type Tubes = Map.Map (Integer, Integer) Int

main :: IO ()
main = do
  tubes <-
    Map.filter (< 9) . Map.map digitToInt . gridMap <$>
    getFile "data/9-puzzle-input"
  let isLow p c = c < minimum (neighs p)
      neighs p = mapMaybe ((tubes !?) . psum p) fourDirs
      lows = Map.filterWithKey isLow tubes
      p1 = sum $ Map.map (+ 1) lows
      llows = Map.fromList $ zip (Map.keys lows) [0,1 ..]
      neigh p =
        mapMaybe (\d -> (, psum p d) <$> tubes !? psum p d) fourDirs & minimum &
        snd
      findGrad b p c = fromMaybe (b ! neigh p) (llows !? p)
      basins = Map.mapWithKey (findGrad basins) tubes
      x:y:z:_ =
        Map.elems llows & map (\c -> mcount (== c) basins) & reverse . sort
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show (x * y * z)
