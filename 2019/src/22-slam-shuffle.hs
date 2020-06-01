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
      p2 = shuffle techniques 2020 119315717514047
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

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

shuffle :: [Technique] -> Integer -> Integer -> Integer
shuffle techniques card d = foldl' (shuffle' d) card techniques
  where isStack NewStack = True
        isStack _ = False

shuffle' :: Integer -> Integer -> Technique -> Integer
shuffle' d card NewStack = d - 1 - card
shuffle' d card (Deal n) = (card * n) `rem` d
shuffle' d card (Cut n) | n >= 0 && card >= n = card - n
                        | n >= 0 = d - n + card
                        | n < 0 = shuffle' d card $ Cut (d + n)
