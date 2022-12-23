import           Control.Applicative  (liftA2)
import           Data.Char            (isAsciiUpper, ord)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           Text.Megaparsec      (choice, some)
import           Text.Megaparsec.Char (newline, printChar)
import           Utils                (Parser, count, decimal, getFile, parseFile, slices, symbol,
                                       word)

dirSizeLimit = 100000

totalDiskSpace = 70000000

unusedSpaceNeeded = 30000000

data Command
  = LS [Files]
  | CDBACK
  | CD String
  deriving (Eq, Show)

data Files
  = File Int String
  | Dir String
  deriving (Eq, Show)

lsParser :: Parser Files
lsParser = choice [symbol "dir" *> (Dir <$> word), File <$> decimal <*> some printChar <* newline]

commandParser :: Parser Command
commandParser =
  choice
    [ symbol "$ cd .." $> CDBACK
    , symbol "$ cd " *> (CD <$> word)
    , symbol "$ ls" *> (LS <$> some lsParser)
    ]

main :: IO ()
main = do
  commands <- parseFile "data/7-puzzle-input" $ symbol "$ cd /" *> some commandParser
  let fileSize (File s _) = s
      fileSize (Dir _)    = 0
      dirName _ (File _ _) = Nothing
      dirName path (Dir s) = Just (s : path)
      walk dirs _ [] = dirs
      walk dirs (p:ps) (CDBACK:xs) = walk dirs ps xs
      walk dirs path ((CD s):xs) = walk dirs (s : path) xs
      walk dirs path ((LS files):xs) = walk dirs' path xs
        where
          dirs' = Map.insert path (size, children) dirs
          size = sum $ map fileSize files
          children = mapMaybe (dirName path) files
      walk _ _ _ = error "should not happen"
      filetree = walk Map.empty ["/"] commands
      sizes =
        Map.mapWithKey (\path (size, children) -> size + sum (map (sizes !) children)) filetree
      p1 = sum $ Map.filter (<= dirSizeLimit) sizes
      neededSpace = unusedSpaceNeeded - (totalDiskSpace - sizes ! ["/"])
      Just p2 = Map.elems sizes & sort & find (>= neededSpace)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
