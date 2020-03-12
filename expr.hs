module Expr where
import Text.Parsec

expr :: Parsec String () String
expr = do {term; many (do {char '+'; term}); return "OK"}

term :: Parsec String () ()
term = do {factor; many (do {char '+'; factor}); return ()}

factor :: Parsec String () ()
factor = do {char '('; expr; char ')'; return ()} <|> do {many1 digit; return ()}
