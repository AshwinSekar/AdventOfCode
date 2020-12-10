module Utils
( readLines,
  splitString,
  slices,
  pairSum,
  getFile,
  parseFile,
  count
) where

import Data.Void
import Text.Megaparsec (ParsecT, runParserT, errorBundlePretty)

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
      then return $ reverse lines
      else readLines (line:lines)


getFile :: String -> IO [String]
getFile file = lines <$> readFile file

parseFile :: String -> ParsecT Void String IO a -> IO a
parseFile file parser = do
  input <- readFile file
  e <- runParserT parser "" input
  return $ case e of
    Left x  -> error $ errorBundlePretty x
    Right x -> x

splitString :: (Char -> Bool) -> String -> [String]
splitString p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitString p s''
                        where (w, s'') = break p s'

slices :: Int -> [a] -> [[a]]
slices size s = case splitAt size s of
                    (x, []) -> [x]
                    (x, y) -> x : slices size y

pairSum :: Num a => (a, a) -> a
pairSum (x, y) = x + y

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
