import           Control.Applicative (liftA2)
import           Data.Char           (isAsciiUpper, ord)
import           Data.Function       ((&))
import           Data.Functor        (($>))
import           Data.List           (intersect, sort)
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (choice, some)
import           Utils               (Parser, count, decimal, getFile, parseFile, slices, symbol)

data Status
  = Searching Int
  | Found Int

main :: IO ()
main = do
  input <- head <$> getFile "data/6-puzzle-input"
  let findMarker _ (Found i, _) _ = (Found i, [])
      findMarker size (Searching i, xs) x =
        if Set.size (Set.fromList newL) == size
          then (Found i', [])
          else (Searching i', newL)
        where
          i' = i + 1
          newL = take size $ x : xs
      (Found p1, _) = foldl (findMarker 4) (Searching 0, []) input
      (Found p2, _) = foldl (findMarker 14) (Searching 0, []) input
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
