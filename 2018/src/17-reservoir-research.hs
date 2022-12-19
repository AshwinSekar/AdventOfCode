import           Control.Monad
import           Control.Monad.ST.Trans
import qualified Data.Array                 as Array
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Ix
import           Data.List
import           Data.Tuple.Extra
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char       (space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Utils

type Parser = ParsecT Void String IO

type Clays a = ([(a, (a, a))], [(a, (a, a))])

data Clay a
  = X a (a, a)
  | Y a (a, a)

data Status
  = Clay
  | LRest
  | RRest
  | Rest
  | Flow
  | Sand
  deriving (Eq)

isLRest x = isRest x || x == LRest

isRRest x = isRest x || x == RRest

isRest Clay = True
isRest Rest = True
isRest _    = False

clayParser :: Integral a => Parser (Clay a)
clayParser = do
  xy <- choice [string "x" $> X, string "y" $> Y]
  xy <$> between (string "=") (string ", ") decimal <*>
    ((,) <$> between (anySingle *> string "=") (string "..") decimal <*> decimal)

parser :: Integral a => Parser (Clays a)
parser = many clayParser <&> partition isX <&> both (map unwrap)
  where
    isX (X _ _) = True
    isX (Y _ _) = False
    unwrap (X k v) = (k, v)
    unwrap (Y k v) = (k, v)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- join <$> readLines []
  Right clays <- runParserT parser "" input
  grid <- runSTArray $ scanWater clays (findMaxY clays)
  let grid' =
        Array.assocs grid & filter ((/= Sand) . snd) & filter ((/= Clay) . snd) &
        filter ((>= findMinY clays) . snd . fst)
      grid'' = filter ((== Rest) . snd) grid'
  putStrLn $ "Part 1: " ++ show (length grid')
  putStrLn $ "Part 2: " ++ show (length grid'')
  where
    findMinY (xClay, yClay) =
      min (minimum $ map (fst . snd) xClay) (minimum $ map fst yClay)
    findMaxY (xClay, yClay) =
      max (maximum $ map (snd . snd) xClay) (maximum $ map fst yClay)

scanWater ::
     (Integral a, Ix a) => Clays a -> a -> STT s IO (STArray s (a, a) Status)
scanWater (xClay, yClay) maxY = do
  grid <- newSTArray ((0, 0), (3000, maxY + 1)) Sand
  let start = (500, 0)
      write x pos = writeSTArray grid pos x
      read pos = readSTArray grid pos
      go pos@(x, y)
        | y > maxY = return ()
        | otherwise = do
          cur <- read pos
          left <- read (x - 1, y)
          right <- read (x + 1, y)
          case (cur, isLRest left && isRRest right) of
            (Sand, _) -> go' pos
            (Rest, _) -> return ()
            (Clay, _) -> return ()
            (Flow, _) -> return ()
            (_, True) -> write Rest pos >> go (x - 1, y) >> go (x + 1, y)
            _         -> return ()
      go' pos@(x, y) = do
        write Flow pos
        -- Down first
        go (x, y + 1)
        down <- read (x, y + 1)
        when (isRest down) $ go (x - 1, y) >> go (x + 1, y)
        left <- read (x - 1, y)
        right <- read (x + 1, y)
        case (isRest down, isLRest left, isRRest right) of
          (True, True, False) -> write LRest pos
          (True, False, True) -> write RRest pos
          (True, True, True) -> write Rest pos >> go (x - 1, y) >> go (x + 1, y)
          _ -> return ()
  traverse_
    (\(x, (y0, y1)) -> forM_ (range ((x, y0), (x, y1))) (write Clay))
    xClay
  traverse_
    (\(y, (x0, x1)) -> forM_ (range ((x0, y), (x1, y))) (write Clay))
    yClay
  go start
  return grid
