import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as Map
import Data.Ratio
import Data.Tuple.Extra

import Utils
import Debug.Trace

type Edges = Map.Map String [(String, (Integer, Integer))] -- u -> [(vi, (xi, yi))] where each xi vi = yi u + ...
type ChemicalState = (Map.Map String Integer, Map.Map String Rational, [String])

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let edges = Map.fromListWith (++) $ concatMap parse input
      (p1, ratio) = evalState (chemistry edges) (Map.empty, Map.empty, [])
      trillRatio = 1000000000000 % 1 / ratio
      p2 = numerator trillRatio `div` denominator trillRatio
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2


chemistry :: Edges -> State ChemicalState (Integer, Rational)
chemistry edges = do
  topSort edges
  l <- reverse <$> gets thd3
  mapM_ (computeQuantity edges) l
  p1 <- (Map.! "ORE") <$> gets fst3
  p2 <- (Map.! "ORE") <$> gets snd3
  return (p1, p2)


parse :: String -> [(String, [(String, (Integer, Integer))])]
parse s =
  let (l, r) = break (== '=') s
      ings = splitString (== ',') l
      ings' = map splitRule ings
      (v, x) = splitRule $ drop 2 r
  in  map (\(u, y) -> (u, [(v, (x, y))])) ings'
  where splitRule t = let [c, r] = words t in (r, read c)

topSort :: Edges -> State ChemicalState ()
topSort edges =
  let
    dfs :: String -> State ChemicalState ()
    dfs s = do
      visited <- gets fst3
      case (Map.member s visited, edges Map.!? s) of
        (True, _) -> return ()
        (_, Nothing) -> addTop s
        (_, Just edges') -> mapM_ (dfs . fst) edges' >> visit s >> addTop s

    visit :: String -> State ChemicalState ()
    visit s = do
      (visited, x, top) <- get
      put (Map.insert s 0 visited, x, top)

    addTop :: String -> State ChemicalState ()
    addTop s = do
      (visited, x, top) <- get
      put (visited, x, s:top)
  in dfs "ORE"

computeQuantity :: Edges -> String -> State ChemicalState ()
computeQuantity edges s = do
  (quantities, quantitiesR, top) <- get
  let
    computeEdgeQ (v, (x, y)) = ceiling (fromIntegral vq / fromIntegral x) * y
      where vq = quantities Map.! v

    computeEdgeQ' (v, (x, y)) = vq * (y % x)
      where vq = quantitiesR Map.! v

    q = case edges Map.!? s of
            Nothing -> 1 -- Fuel
            Just edges' -> sum $ map computeEdgeQ edges'
    q' = case edges Map.!? s of
            Nothing -> 1 % 1 -- Fuel
            Just edges' -> sum $ map computeEdgeQ' edges'
  put (Map.insert s q quantities, Map.insert s q' quantitiesR, top)

-- type Edges = Map.Map String [(String, (Integer, Integer))] -- u -> [(vi, (xi, yi))] where each xi vi = yi u + ...
computeFuel :: Edges -> Integer -> State ChemicalState Integer
computeFuel edges ore = return 0
