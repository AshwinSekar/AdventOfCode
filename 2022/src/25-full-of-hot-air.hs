import           Data.Char            (digitToInt, intToDigit)
import           Data.Functor         (($>))
import           Data.List            (unfoldr)
import           Text.Megaparsec      (choice, some)
import           Text.Megaparsec.Char (char, digitChar, newline)
import           Utils                (Parser, parseFile)

fuelParser :: Parser [Int]
fuelParser = some $ choice [digitToInt <$> digitChar, char '=' $> -2, char '-' $> -1]

f2d :: [Int] -> Int
f2d = foldl (\a x -> 5 * a + x) 0

d2f :: Int -> [Int]
d2f = reverse . unfoldr d2f'
  where
    d2f' 0 = Nothing
    d2f' d =
      let (r, m) = d `divMod` 5
       in if m < 3
            then Just (m, r)
            else Just (m - 5, r + 1)

f2s :: [Int] -> String
f2s []       = []
f2s ((-2):f) = '=' : f2s f
f2s ((-1):f) = '-' : f2s f
f2s (x:f)    = intToDigit x : f2s f

main :: IO ()
main = do
  fuels <- parseFile "data/25-puzzle-input" (some (fuelParser <* newline))
  putStrLn $ f2s . d2f $ sum (map f2d fuels)
