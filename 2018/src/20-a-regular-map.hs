 {-# LANGUAGE NamedFieldPuns, ViewPatterns, TypeApplications #-}
import Utils

import Control.Monad.State.Lazy

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (space, char)
import Text.Megaparsec.Char.Lexer (decimal)

import Debug.Trace

type Parser a = ParsecT Void String (StateT (PathState a) IO)
type Point a = (a, a)
data PathState a = PathState { visited :: Map (Point a) a
                             , paths   :: Map (Point a) a } deriving ( Show )

visit :: Integral a => Parser a ()
visit = lift $ modify (\ s -> s{visited = visit' s})
  where visit' (PathState v p) = Map.unionWith min v p

move :: Integral a => Point a -> Parser a ()
move (x, y) = lift $ modify (\ s -> s{paths = move' s})
  where move' PathState{paths} = (+1) <$> Map.mapKeysWith min pPlus paths
        pPlus (x', y') = (x + x', y + y')

parser :: Integral a => Parser a (PathState a)
parser = visit *> choice [
    between (char '(') (char ')') splitParser *> parser
  , char 'N' *> move ( 0,  1) *> parser
  , char 'W' *> move (-1,  0) *> parser
  , char 'S' *> move ( 0, -1) *> parser
  , char 'E' *> move ( 1,  0) *> parser
  , char '^' *> parser
  , lookAhead (char '|') *> lift get
  , lookAhead (char ')') *> lift get
  , char '$' *> lift get
  , eof      *> lift get ]

splitParser :: Integral a => Parser a ()
splitParser = do
  s <- lift get
  states <- sepBy (lift (put s) *> parser) (char '|')
  put $ foldl1 combine states
  where combine (PathState v p) (PathState v' p') = PathState (Map.unionWith min v v') (Map.unionWith min p p')


main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  Right (visited -> dists) <- evalStateT (runParserT (parser @Int) "" input) initial
  putStrLn $ "Part 1: " ++ show (maximum dists)
  putStrLn $ "Part 2: " ++ show (Map.size $ Map.filter (>= 1000) dists)
  where initial = PathState Map.empty $ Map.singleton (0, 0) 0
