import           Utils               hiding (count)

import           Control.Applicative (liftA2, (<|>))

import           Data.Function       ((&))
import           Data.Map            ((!), (!?))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (count, some)

segmentParser :: Parser ([String], [String])
segmentParser = liftA2 (,) (some word <* symbol "|") (count 4 word)

smapping :: Map.Map Int String
smapping =
  Map.fromList
    [ (0, "abcefg")
    , (1, "cf")
    , (2, "acdeg")
    , (3, "acdfg")
    , (4, "bcdf")
    , (5, "abdfg")
    , (6, "abdefg")
    , (7, "acf")
    , (8, "abcdefg")
    , (9, "abcdfg")
    ]

main :: IO ()
main = do
  inputs <- parseFile "data/8-puzzle-input" $ some segmentParser
  let p1 = sum $ map (length . filter (\s -> length s `elem` [2, 4, 3, 7]) . snd) inputs
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show (score (head init) p2)
