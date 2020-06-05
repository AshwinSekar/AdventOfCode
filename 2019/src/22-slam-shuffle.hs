import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

import Utils
import Debug.Trace


data Technique = NewStack | Cut Integer | Deal Integer deriving (Show)
type Deck = [Integer]

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let techniques = map parse input
      p1Deck = [0..10006]
      shuffled = foldl shuffleNaive p1Deck techniques
      Just p1 = elemIndex 2019 shuffled
      deckSize = 119315717514047
      (s, m) = shuffle techniques deckSize 101741582076661 (0, 1)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show ((s + 2020 * m) `rem` deckSize)

parse :: String -> Technique
parse "deal into new stack" = NewStack
parse s =
  let (s', digits) = break isDigit' s
  in case stripPrefix "cut" s' of
    Just _ -> Cut $ read digits
    Nothing -> Deal $ read digits
  where isDigit' d = isDigit d || d == '-'


shuffleNaive :: Deck -> Technique -> Deck
shuffleNaive deck NewStack = reverse deck
shuffleNaive deck (Deal n) = deal n deck
shuffleNaive deck (Cut n) | n >= 0 = let (a, b) = splitAt (fromIntegral n) deck in b ++ a
                          | n < 0 = shuffleNaive deck $ Cut (fromIntegral (length deck) + n)


deal :: Integer -> [Integer] -> [Integer]
deal n deck =
  let d = fromIntegral $ length deck
      deal' (i, _) m = ((i + n) `rem` d, m)
      indexD = tail $ scanl deal' (-n, 0) deck
  in map snd $ sort indexD

shuffle :: [Technique] -> Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
shuffle techniques d n deck = ((s * (1 - m') * inv (1 - m + d) d) `rem` d, m')
  where (s, m) = foldl' (shuffle' d) deck techniques
        m' = modPow m n d

shuffle' :: Integer -> (Integer, Integer) -> Technique -> (Integer, Integer)
shuffle' d (s, m) NewStack = ((s + (d - 1) * m) `rem` d , -m)
shuffle' d (s, m) (Deal n) = (s, (m * inv n d) `rem` d)
shuffle' d (s, m) (Cut n) = ((s + m * (n + d)) `rem` d, m)

inv n d = (fst (eGCD n d) + d) `rem` d

eGCD 0 b = (0, 1)
eGCD a b = let (s, t) = eGCD (b `mod` a) a
            in (t - (b `div` a) * s, s)

modPow b e 1 = 0
modPow b 1 m = b `rem` m
modPow b e m
  | even e = (x * x) `rem` m
  | odd e = (b * modPow b (e - 1) m) `rem` m
  where x = modPow b (e `div` 2) m
