import Utils

import Control.Monad

import Data.Char
import Data.List
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (space, newline, string)
import Text.Megaparsec.Char.Lexer (signed, decimal)

type Parser = Parsec Void String

parser :: (Num a) => Parser [Star a]
parser = (`sepEndBy` newline) $ Star
  <$> (space *> signed space decimal)
  <*> (string "," *> signed space decimal)
  <*> (string "," *> signed space decimal)
  <*> (string "," *> signed space decimal)


data Star a = Star { x :: a
                   , y :: a
                   , z :: a
                   , t :: a} deriving (Show, Eq, Ord)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- unlines <$> readLines []
  Just stars <- return $ parseMaybe (parser @Int) input
  putStrLn $ "Part 1: " ++ show (length $ scc (buildGraph 3 stars))

buildGraph :: (Num a, Ord a) => a -> [Star a] -> Gr (Star a) a
buildGraph tol stars = mkGraph nodes edges
  where nodes = zip [0..] stars
        edges = do
          (i, Star x y z t) <- nodes
          (j, Star x' y' z' t') <- nodes
          let d = abs (x - x') + abs (y - y') + abs (z - z') + abs (t - t')
          guard (d <= tol)
          return (i, j, d)
