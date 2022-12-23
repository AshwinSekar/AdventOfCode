import           Control.Applicative  (liftA2)
import           Data.Char            (digitToInt, isAsciiUpper, ord)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           Text.Megaparsec      (choice, some)
import           Text.Megaparsec.Char (newline, printChar)
import           Utils                (Parser, count, decimal, getFile, gridMap, parseFile, psum,
                                       slices, symbol, word)

main :: IO ()
main = do
  trees <- Map.map digitToInt . gridMap <$> getFile "data/8-puzzle-input"
  let look dir los pos height =
        case trees !? pos' of
          Nothing -> (-1, 0)
          Just h  -> max (h, 1) (psum (0, 1) (los ! pos'))
        where
          pos' = psum pos dir
  let losL = Map.mapWithKey (look (-1, 0) losL) trees
      losR = Map.mapWithKey (look (1, 0) losR) trees
      losU = Map.mapWithKey (look (0, -1) losU) trees
      losD = Map.mapWithKey (look (0, 1) losD) trees
      los = [losL, losR, losU, losD]
  let visible pos height = any ((height >) . fst . (! pos)) los
      scenic pos = product $ map (snd . (! pos)) los
      p1 = Map.size $ Map.filterWithKey visible trees
      p2 = maximum $ map scenic $ Map.keys trees
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
