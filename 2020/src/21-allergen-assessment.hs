import           Control.Applicative  (liftA2)
import           Data.List
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Text.Megaparsec      (sepBy, some, someTill)
import           Text.Megaparsec.Char (letterChar)
import           Utils

parser :: Parser ([String], [String])
parser =
  liftA2
    (,)
    (someTill (lexeme $ some letterChar) $ symbol "(contains")
    (some letterChar `sepBy` symbol "," <* symbol ")")

solve ::
     Map.Map String (Set.Set String)
  -> Map.Map String String
  -> Map.Map String String
solve poss mapping
  | all Set.null poss = mapping
  | otherwise = solve poss' mapping'
  where
    solved = Map.filter ((== 1) . Set.size) poss
    mapping' =
      Map.foldlWithKey
        (\m a (Set.toList -> [i]) -> Map.insert a i m)
        mapping
        solved
    poss' = Map.map (\s -> s Set.\\ Set.unions solved) poss

main :: IO ()
main = do
  input <- parseFile "data/21-puzzle-input" $ some parser
  let poss =
        Map.fromListWith
          Set.intersection
          [(a, Set.fromList ings) | (ings, allergs) <- input, a <- allergs]
  putStrLn $
    "Part 1: " ++
    show
      (length
         [i | (ings, allergs) <- input, i <- ings, i `notElem` Set.unions poss])
  putStrLn $ "Part 2: " ++ intercalate "," (Map.elems $ solve poss Map.empty)
