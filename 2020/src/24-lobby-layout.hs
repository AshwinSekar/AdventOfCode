import Data.Functor
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Maybe
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char (newline, string)
import Utils

parser :: Parser [[(Int, Int)]]
parser =
  some
    ( choice
        [ string "e" $> (0, 2),
          string "w" $> (0, -2),
          try (string "nw") $> (2, -1),
          try (string "ne") $> (2, 1),
          try (string "sw") $> (-2, -1),
          try (string "se") $> (-2, 1)
        ]
    )
    `sepEndBy1` newline

step :: Set.HashSet (Int, Int) -> Set.HashSet (Int, Int)
step tiles = Set.fromList $ mapMaybe go adjs
  where
    neighs = psum <$> [(0, 2), (0, -2), (2, -1), (2, 1), (-2, -1), (-2, 1)]
    adjs = neighs <*> Set.toList tiles
    go p = case (p `Set.member` tiles, count (`Set.member` tiles) (neighs <*> [p])) of
      (True, 1) -> Just p
      (True, 2) -> Just p
      (False, 2) -> Just p
      _ -> Nothing

main :: IO ()
main = do
  input <- parseFile "data/24-puzzle-input" parser
  let tiles = Map.fromListWith xor $ map ((,True) . foldl1 psum) input
      init = Map.keysSet $ Map.filter id tiles
      final = iterate step init !! 100
  putStrLn $ "Part 1: " ++ show (Set.size init)
  putStrLn $ "Part 2: " ++ show (Set.size final)
