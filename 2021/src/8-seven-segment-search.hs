import           Control.Applicative (liftA2, (<|>))
import           Control.Monad       (join)
import           Data.Function       ((&))
import           Data.List           (partition)
import           Data.Map            ((!), (!?))
import qualified Data.Map            as Map
import           Data.Set            ((\\))
import qualified Data.Set            as Set
import           Text.Megaparsec     (count, some)
import           Utils               hiding (count)

segmentParser :: Parser ([Set.Set Char], [Set.Set Char])
segmentParser =
  liftA2
    (,)
    (some (Set.fromList <$> word) <* symbol "|")
    (count 4 (Set.fromList <$> word))

main :: IO ()
main = do
  inputs <- parseFile "data/8-puzzle-input" $ some segmentParser
  let p1 =
        sum $
        map
          (length . filter (\s -> Set.size s `elem` [2, 4, 3, 7]) . snd)
          inputs
      p2 = sum $ map solve inputs
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

solve :: ([Set.Set Char], [Set.Set Char]) -> Int
solve (ins, out) = read $ join (map (show . ((findCoding ins) !)) out)

findCoding :: [Set.Set Char] -> Map.Map (Set.Set Char) Int
findCoding ins =
  let sizeFilter n = filter (\s -> Set.size s == n) ins
      [[s1], [s4], [s7], [s8]] = sizeFilter <$> [2, 4, 3, 7]
      [s235, s069] = sizeFilter <$> [5, 6]
      ([s3], s25) = partition (\s -> Set.size (s1 \\ s) == 0) s235
      ([s5], [s2]) = partition (\s -> Set.size (s4 \\ s) == 1) s25
      ([s9], s06) = partition (\s -> Set.size (s4 \\ s) == 0) s069
      ([s0], [s6]) = partition (\s -> Set.size (s7 \\ s) == 0) s06
   in Map.fromList
        [ (s0, 0)
        , (s1, 1)
        , (s2, 2)
        , (s3, 3)
        , (s4, 4)
        , (s5, 5)
        , (s6, 6)
        , (s7, 7)
        , (s8, 8)
        , (s9, 9)
        ]
