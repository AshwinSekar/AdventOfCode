import Utils

import Control.Monad
import Control.Monad.STM

import Data.Char
import Data.Foldable
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import qualified Data.STM.LinkedList as LL

import Debug.Trace

type Reaction = LL.LinkedList Char


main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/5-puzzle-input"
  p1 <- atomically $ parse input >>= collapse >>= LL.length
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2


parse :: String -> STM Reaction
parse s = do
  ll <- LL.empty
  traverse_ (`LL.append` ll) s
  return ll

toString :: Reaction -> STM String
toString = LL.toList




collapse :: Reaction -> STM Reaction
collapse ll = do
  st <- LL.start ll
  case st of
    Nothing -> return ll
    Just n -> collapse' ll n

collapse' :: Reaction -> LL.Node Char -> STM Reaction
collapse' ll n = do
  nP <- LL.prev n
  nN <- LL.next n
  let del = LL.delete n >> LL.delete (fromJust nN)
  case (eqOpCase nVal . LL.value <$> nN, nP)  of
    (Nothing, _) -> return ll
    (Just False, _) -> collapse' ll (fromJust nN)
    (Just True, Nothing) -> del >> collapse ll
    (Just True, Just nP) -> del >> collapse' ll nP
  where nVal = LL.value n

eqOpCase :: Char -> Char -> Bool
eqOpCase a b = (toUpper a == toUpper b) && ((isUpper a && isLower b) || (isLower a && isUpper b))
