import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Utils

data WireSegment
  = Horizontal
      { y  :: Integer
      , x1 :: Integer
      , x2 :: Integer
      , s  :: Integer
      }
  | Vertical
      { x  :: Integer
      , y1 :: Integer
      , y2 :: Integer
      , s  :: Integer
      }
  deriving (Show, Ord, Eq)

main :: IO ()
main = do
  putStrLn "Input:"
  wire1 <- splitString (== ',') <$> getLine
  wire2 <- splitString (== ',') <$> getLine
  let w1 = segment wire1
      w2 = segment wire2
      ints = findIntersections w1 w2
      p1 = manhattanMin ints
      p2 = stepMin ints
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 1: " ++ show p2

segment :: [String] -> [WireSegment]
segment wire = scanl1 segment' $ map (toSegment . parse) wire
  where
    parse s = (head s, read $ drop 1 s)

toSegment :: (Char, Integer) -> WireSegment
toSegment ('R', d) = Horizontal 0 0 d d
toSegment ('L', d) = Horizontal 0 0 (-d) d
toSegment ('U', d) = Vertical 0 0 d d
toSegment ('D', d) = Vertical 0 0 (-d) d

segment' :: WireSegment -> WireSegment -> WireSegment
segment' (Vertical x _ y s) (Horizontal _ _ d s') =
  Horizontal y x (x + d) (s + s')
segment' (Horizontal y _ x s) (Vertical _ _ d s') =
  Vertical x y (y + d) (s + s')

findIntersections ::
     [WireSegment] -> [WireSegment] -> [(Integer, Integer, Integer, Integer)]
findIntersections w1 w2 = catMaybes $ liftM2 findIntersection w1 w2

findIntersection ::
     WireSegment -> WireSegment -> Maybe (Integer, Integer, Integer, Integer)
findIntersection w@Horizontal {} w'@Vertical {} = findIntersection w' w
findIntersection (Vertical x y1 y2 s1) (Horizontal y x1 x2 s2) =
  if between x1 x x2 && between y1 y y2
    then Just (x, y, s1 - abs (y2 - y), s2 - abs (x2 - x))
    else Nothing
findIntersection _ _ = Nothing

between x y z
  | x <= y = y <= z
  | x >= y = y >= z

manhattanMin :: [(Integer, Integer, Integer, Integer)] -> Integer
manhattanMin ints = minimum $ filter (> 0) (map manhattan ints)
  where
    manhattan (x, y, _, _) = abs x + abs y

stepMin :: [(Integer, Integer, Integer, Integer)] -> Integer
stepMin ints = minimum $ filter (> 0) (map step ints)
  where
    step (_, _, s1, s2) = s1 + s2
