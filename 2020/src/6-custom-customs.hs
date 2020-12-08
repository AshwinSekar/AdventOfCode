import Utils

import Control.Monad
import Control.Monad.ST

import Data.Char
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import Data.List.Split
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (spaceChar, newline, string, lowerChar, asciiChar, char)
import Text.Megaparsec.Char.Lexer (decimal)

import Debug.Trace

type Parser = ParsecT Void String IO

main :: IO ()
main = do
  input <- getFile "data/6-puzzle-input"
  let groups = splitWhen (=="") input
      p1 = sum $ map anyYes groups
      p2 = sum $ map allYes groups
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2


anyYes answers = map Set.fromList answers
                  & Set.unions
                  & Set.size

allYes [] = 0
allYes answers = map Set.fromList answers
                  & foldl1' Set.intersection
                  & Set.size
