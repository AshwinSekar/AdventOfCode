import           Control.Applicative  (liftA2, liftA3)
import           Data.Function        ((&))
import           Data.Functor
import           Data.List
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec      (sepBy1, some, someTill)
import           Text.Megaparsec.Char (printChar)
import           Utils

type Rule = ((Int, Int), (Int, Int))

ruleParser :: Parser (String, Rule)
ruleParser =
  liftA2
    (,)
    (someTill printChar (symbol ":"))
    (liftA2
       (,)
       (liftA2 (,) (decimal <* symbol "-") (decimal <* symbol "or "))
       (liftA2 (,) (decimal <* symbol "-") decimal))

parser :: Parser (Map.Map String Rule, [Int], [[Int]])
parser =
  liftA3
    (,,)
    (Map.fromList <$> someTill ruleParser (symbol "your ticket:"))
    (decimal `sepBy1` symbol ",")
    (symbol "nearby tickets:" *> some (decimal `sepBy1` symbol ","))

main :: IO ()
main = do
  (rules, yours, nearby) <- parseFile "data/16-puzzle-input" parser
  let validRule x ((a, b), (c, d)) = a <= x && x <= b || c <= x && x <= d
      validTicket t =
        listToMaybe $
        mapMaybe
          (\x ->
             if any (validRule x) rules
               then Nothing
               else Just x)
          t
      addField (rules, known) f =
        case Map.assocs $ Map.filter (\r -> all (`validRule` r) f) rules of
          [(s, r)] -> (Map.delete s rules, Map.insert s (head f) known)
          _        -> (rules, known)
      fields = transpose $ yours : filter (isNothing . validTicket) nearby
  putStrLn $ "Part 1: " ++ show (sum $ mapMaybe validTicket nearby)
  putStrLn $
    "Part 2: " ++
    show
      (iterate (\m -> foldl addField m fields) (rules, Map.empty) &
       find (Map.null . fst) <&> snd <&>
       Map.filterWithKey (\s _ -> "departure" `isPrefixOf` s) <&>
       product &
       fromJust)
