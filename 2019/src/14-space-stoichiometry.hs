import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as Map

import Utils
import Debug.Trace

type Edges = Map.Map String [(String, (Integer, Integer))] -- u -> [(vi, (xi, yi))] where each xi vi = yi u + ...
type ChemicalState = (Map.Map String Integer, [String])

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let edges = Map.fromListWith (++) $ concatMap parse input
      (p1, p2) = evalState (chemistry edges) (Map.empty, [])
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2


chemistry :: Edges -> State ChemicalState (Integer, Integer)
chemistry edges = do
  topSort edges
  l <- reverse <$> gets snd
  mapM_ (computeQuantity edges) l
  p1 <- (Map.! "ORE") <$> gets fst
  p2 <- computeFuel edges 1000000000000
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
      visited <- gets fst
      case (Map.member s visited, edges Map.!? s) of
        (True, _) -> return ()
        (_, Nothing) -> addTop s
        (_, Just edges') -> mapM_ (dfs . fst) edges' >> visit s >> addTop s

    visit :: String -> State ChemicalState ()
    visit s = do
      (visited, top) <- get
      put (Map.insert s 0 visited, top)

    addTop :: String -> State ChemicalState ()
    addTop s = do
      (visited, top) <- get
      put (visited, s:top)
  in dfs "ORE"

computeQuantity :: Edges -> String -> State ChemicalState ()
computeQuantity edges s = do
  (quantities, top) <- get
  let
    computeEdgeQ (v, (x, y)) = ceiling (fromIntegral vq / fromIntegral x) * y
      where vq = quantities Map.! v

    q = case edges Map.!? s of
            Nothing -> 1 -- Fuel
            Just edges' -> sum $ map computeEdgeQ edges'
  put (Map.insert s q quantities, top)

-- type Edges = Map.Map String [(String, (Integer, Integer))] -- u -> [(vi, (xi, yi))] where each xi vi = yi u + ...
computeFuel :: Edges -> Integer -> State ChemicalState Integer
computeFuel edges ore = return 0
