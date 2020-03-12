-- http://utotch.blogspot.com/2011/12/haskell-parser.html
{-
expr = term '+' expr | term
term = factor '*' term | factor
factor = '(' expr ')' | nat
nat = digit+
digit = '0'..'9'
-}
import Control.Applicative ((<$>),(*>),(<*),pure)
import Text.Parsec (char,digit,(<|>),parseTest)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)

data Exp = Add Exp Exp
  | Mul Exp Exp
  | Nat Int
  deriving Show


-- expr = term '+' expr | term
expr :: Parser Exp
expr = do
  t <- term
  (Add t <$> (char '+' *> expr)) <|> pure t

-- term = factor '*' term | factor
term :: Parser Exp
term = do
  f <- factor
  (Mul f <$> (char '*' *> term)) <|> pure f

-- factor = '(' expr ')' | nat
factor :: Parser Exp
factor = (char '(' *> (expr <* char ')')) <|> nat

-- nat = ('0'..'9')+
nat = do
  s <- many1 digit
  return (Nat (read s :: Int))

{-
parseTest expr "1+2*3"
parseTest expr "(1+2)*3"
-}
