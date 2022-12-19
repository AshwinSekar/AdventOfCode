module Utils
  ( readLines
  , splitString
  , slices
  ) where

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
    then return $ reverse lines
    else readLines (line : lines)

splitString :: (Char -> Bool) -> String -> [String]
splitString p s =
  case dropWhile p s of
    "" -> []
    s' -> w : splitString p s''
      where (w, s'') = break p s'

slices :: Int -> [a] -> [[a]]
slices size s =
  case splitAt size s of
    (x, []) -> [x]
    (x, y)  -> x : slices size y
