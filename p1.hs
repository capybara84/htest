-- http://utotch.blogspot.com/2011/12/haskell-parser.html
{-
expr = term '+' expr | term
term = factor '*' term | factor
factor = '(' expr ')' | nat
nat = ('0'..'9')+
-}

import Text.Parsec
import Text.Parsec.String

data Exp = Add Exp Exp
  | Mul Exp Exp
  | Nat Int
  deriving Show

-- expr = term '+' expr | term
expr :: Parser Exp
expr = do
  t <- term
  do
    char '+'
    e <- expr
    return (Add t e)
    <|> return t

-- term = factor '*' term | factor
term :: Parser Exp
term = do
  f <- factor
  do
    char '*'
    t <- term
    return (Mul f t)
    <|> return f

-- factor = '(' expr ')' | nat
factor :: Parser Exp
factor = do
  char '('
  e <- expr
  char ')'
  return e
  <|> nat

-- nat = ('0'..'9')+
nat = do
  s <- many1 digit
  return (Nat (read s :: Int))

{-
parseTest expr "1+2*3"
parseTest expr "(1+2)*3"
-}
