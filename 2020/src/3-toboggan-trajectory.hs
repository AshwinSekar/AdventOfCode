import           Control.Monad
import           Control.Monad.ST
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char       (letterChar, newline, space, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Utils

type Parser = ParsecT Void String IO

type Policy a = (a, a, Char)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- zip [0,1 ..] <$> readLines []
  let slope :: (Int, Int) -> Int
      slope (r, d) =
        input &
        filter (\(i, str) -> i `rem` d == 0) <&>
        (\(i, str) -> str !! (r * (i `div` d) `rem` length str)) &
        filter (== '#') &
        length
      p1 = slope (3, 1)
      p2 = product $ map slope [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
