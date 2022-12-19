import           Data.Function ((&))
import           Data.List     (sort)
import           Data.Map      ((!))
import qualified Data.Map      as Map
import           Data.Maybe    (mapMaybe)
import qualified Data.Set      as Set
import           Utils

main :: IO ()
main = do
  input <- getFile "data/10-puzzle-input"
  let p1 = sum $ mapMaybe (go []) input
      scores = mapMaybe (go' []) input
      p2 = sort scores !! (length scores `div` 2)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

go :: String -> String -> Maybe Int
go _ [] = Nothing
go a ('(':xs) = go (')' : a) xs
go a ('[':xs) = go (']' : a) xs
go a ('{':xs) = go ('}' : a) xs
go a ('<':xs) = go ('>' : a) xs
go (x:xs) (y:ys) =
  if y == x
    then go xs ys
    else Just (score y)

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

go' :: String -> String -> Maybe Int
go' a [] = Just $ foldl (\s c -> s * 5 + score' c) 0 a
go' a ('(':xs) = go' (')' : a) xs
go' a ('[':xs) = go' (']' : a) xs
go' a ('{':xs) = go' ('}' : a) xs
go' a ('<':xs) = go' ('>' : a) xs
go' (x:xs) (y:ys) =
  if y == x
    then go' xs ys
    else Nothing

score' :: Char -> Int
score' ')' = 1
score' ']' = 2
score' '}' = 3
score' '>' = 4
