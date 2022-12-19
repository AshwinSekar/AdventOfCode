import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.List
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char       (asciiChar, char, lowerChar,
                                             newline, spaceChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Utils

type Parser = ParsecT Void String IO

bsp :: String -> (Int, Int)
bsp s =
  ( bin2Dec $
    map
      (\c ->
         if c == 'F'
           then 0
           else 1)
      row
  , bin2Dec $
    map
      (\c ->
         if c == 'L'
           then 0
           else 1)
      col)
  where
    bin2Dec = foldl' (\a b -> a * 2 + b) 0
    (row, col) = splitAt 7 s

main :: IO ()
main = do
  input <- readLines []
  let convert = map bsp input
      ids = Set.fromList $ map (\(r, c) -> r * 8 + c) convert
      p1 = maximum ids
      [p2] =
        filter
          (\s ->
             Set.member (s + 1) ids &&
             Set.member (s - 1) ids && not (Set.member s ids))
          [0 .. p1]
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
