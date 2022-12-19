import           Control.Applicative (liftA2)
import qualified Data.Set            as Set
import           Text.Megaparsec     (some)
import           Utils

parser :: Parser ([Int], [Int])
parser =
  liftA2
    (,)
    (symbol "Player 1:" *> some decimal)
    (symbol "Player 2:" *> some decimal)

play :: [Int] -> [Int] -> Int
play [] d2 = sum $ zipWith (*) [1 ..] (reverse d2)
play d1 [] = sum $ zipWith (*) [1 ..] (reverse d1)
play (x:d1) (y:d2)
  | x > y = play (d1 ++ [x, y]) d2
  | y > x = play d1 (d2 ++ [y, x])

rplay :: Set.Set ([Int], [Int]) -> [Int] -> [Int] -> Either Int Int
rplay seen [] d2 = Right $ sum (zipWith (*) [1 ..] (reverse d2))
rplay seen d1 [] = Left $ sum (zipWith (*) [1 ..] (reverse d1))
rplay seen d1@(x:d1') d2@(y:d2')
  | (d1, d2) `elem` seen = rplay seen d1 []
  | otherwise =
    case ((x <= length d1') && (y <= length d2'), x > y, sub) of
      (True, _, Left _)  -> left
      (True, _, Right _) -> right
      (_, True, _)       -> left
      (_, False, _)      -> right
  where
    sub = rplay Set.empty (take x d1') (take y d2')
    seen' = Set.insert (d1, d2) seen
    left = rplay seen' (d1' ++ [x, y]) d2'
    right = rplay seen' d1' (d2' ++ [y, x])

main :: IO ()
main = do
  (deck1, deck2) <- parseFile "data/22-puzzle-input" parser
  let p1 = play deck1 deck2
      p2 = either id id $ rplay Set.empty deck1 deck2
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
