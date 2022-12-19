import           Data.Bits
import           Data.List
import           Utils

transform :: Integer -> Integer -> Integer
transform sn ls = sn `modPow` ls
  where
    m = 20201227
    modPow b 0 = 1
    modPow b e = t * modPow ((b * b) `mod` m) (shiftR e 1) `mod` m
      where
        t =
          if testBit e 0
            then b `mod` m
            else 1

main :: IO ()
main = do
  let (card, door) = (11239946, 10464955)
      Just ls = find ((== card) . transform 7) [1 ..]
  print $ transform door ls
