import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.List.Extra             hiding (splitOn)
import           Data.List.Split
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Debug.Trace
import           System.Console.ANSI
import           Utils

type Point = (Int, Int)

type Velocity = (Int, Int)

main :: IO ()
main = do
  putStrLn "Input:"
  stars <- parse <$> readLines []
  let Just p2 = map (step stars) [0 ..] & findIndex msg
      p1 = Map.keys $ step stars p2
  putStrLn "Part 1: "
  printStar p1
  putStrLn $ "Part 2: " ++ show p2

parse :: [String] -> Map Point Velocity
parse s =
  map ((((\[p, v] -> (p, v)) . map parse') . splitOn "> ") . dropEnd 1) s &
  Map.fromList
  where
    parse' t = drop 10 t & splitOn ", " & map read & \[x, y] -> (x, y)

step :: Map Point Velocity -> Int -> Map Point Velocity
step stars i = mapKWithV (\p v -> i ^* v ^+ p) stars

msg :: Map Point Velocity -> Bool
msg (Map.keys -> ps) = avgDiff < 0.05 && avgDiff > 0.041
  where
    starDiff (p', d) p = (p, max d $ (^$+) (p ^- p'))
    avgDiff =
      fromIntegral (snd (foldl' starDiff (head ps, 0) ps)) /
      fromIntegral (length ps)

printStar :: [Point] -> IO ()
printStar stars =
  clearScreen >> traverse_ printStar' stars' >> setCursorPosition 50 0
  where
    stars' = map (^- minimum stars) stars
    printStar' (x, y) = setCursorPosition y x >> putChar '#'
