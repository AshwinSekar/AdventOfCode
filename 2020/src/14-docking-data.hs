import Utils hiding (count)

import Data.Char
import Data.Bits
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Tuple
import Data.Tuple.Extra

import Data.Void
import Text.Megaparsec (ParsecT, runParserT, (<|>), sepEndBy, count, between)
import Text.Megaparsec.Char (newline, string, alphaNumChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = ParsecT Void String IO
data Instr = Mask String | Mem Integer Integer

valMask :: String -> [(Integer, Integer)]
valMask s = [both ((s&) . bin2Dec) (0, 1)]
  where bin2Dec c = foldl (\ a b -> if b == 'X' then a * 2 + c else a * 2 + toInteger (digitToInt b)) 0


addrMask :: String -> [(Integer, Integer)]
addrMask = foldl gen [(0, 0)]
  where gen vs '0' = (\ (o, a) -> (2 * o, 2 * a + 1)) <$> vs
        gen vs '1' = (\ (o, a) -> (2 * o + 1, 2 * a + 1)) <$> vs
        gen vs 'X' = vs >>= (\ (o, a) -> [(2 * o + 1, 2 * a + 1), (2 * o, 2 * a)])

parser :: Parser [Instr]
parser = (Mask <$> (string "mask = " *> count 36 alphaNumChar)
     <|> Mem <$> between (string "mem[") (string "] = ") decimal <*> decimal)
     `sepEndBy` newline

run p1 (mem, _) (Mask mask) = (mem, mask & if p1 then valMask else addrMask)
run p1 (mem, masks) (Mem i x) = (foldl write mem masks, masks)
  where write mem (oMask, aMask) = if p1 then Map.insert i (f x) mem else Map.insert (f i) x mem
          where f = (.|. oMask) . (.&. aMask)

main :: IO ()
main = do
  prog <- parseFile "data/14-puzzle-input" parser
  let (p1, p2) = (run True, run False)
                  & both (sum . fst . (\ f -> foldl f(Map.empty, [(0, complement 0)]) prog))
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
