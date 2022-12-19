import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import           Debug.Trace
import           Utils

type PlanetState a = State (Map.Map String Int) a

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let edges = map split input
      adjList = Map.fromList edges
      p1 = totalOrbit adjList
      p2 = totalTransfers adjList
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
  where
    split s =
      let [a, b] = splitString (== ')') s
       in (b, a)

totalOrbit :: Map.Map String String -> Int
totalOrbit adjList = sum orbits
  where
    orbits = execState (mapM_ (orbit adjList) (Map.keys adjList)) Map.empty

orbit :: Map.Map String String -> String -> PlanetState Int
orbit _ "COM" = return 0
orbit adjList planet = do
  orbits <- get
  case orbits Map.!? planet of
    Just x -> return x
    Nothing -> do
      let planet' = adjList Map.! planet
      nOrbit <- (+ 1) <$> orbit adjList planet'
      put $ Map.insert planet nOrbit orbits
      return nOrbit

totalTransfers :: Map.Map String String -> Int
totalTransfers adjList =
  let aYou = reverse $ ancestors adjList "YOU"
      aSan = reverse $ ancestors adjList "SAN"
      uncommons = zipWith meq aYou aSan
      (uYou, uSan) = head $ catMaybes uncommons
      (you, san) = (dropWhile (/= uYou) aYou, dropWhile (/= uSan) aSan)
   in (length you + length san) - 2
  where
    meq a b =
      if a == b
        then Nothing
        else Just (a, b)

ancestors :: Map.Map String String -> String -> [String]
ancestors adjList "COM" = ["COM"]
ancestors adjList x     = x : ancestors adjList (adjList Map.! x)
