import Utils

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec (ParsecT, (<|>), between, some)

data AST = Int Integer | Sum AST AST | Prod AST AST deriving (Show, Eq, Ord)

paren :: Parser AST -> Parser AST
paren = between (symbol "(") (symbol ")")

ops  = [ [InfixL $ Prod <$ symbol "*",
          InfixL $ Sum  <$ symbol "+"] ]
ops' = [ [InfixL $ Sum  <$ symbol "+"],
         [InfixL $ Prod <$ symbol "*"] ]
ast  = makeExprParser (Int <$> decimal <|> paren ast) ops
ast' = makeExprParser (Int <$> decimal <|> paren ast') ops'

eval :: AST -> Integer
eval (Int i)    = i
eval (Sum l r)  = eval l + eval r
eval (Prod l r) = eval l * eval r

main :: IO ()
main = do
  asts  <- parseFile "data/18-puzzle-input" (some ast)
  asts' <- parseFile "data/18-puzzle-input" (some ast')
  putStrLn $ "Part 1: " ++ show (sum $ map eval asts)
  putStrLn $ "Part 2: " ++ show (sum $ map eval asts')
