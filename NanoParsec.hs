module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)]

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

item :: Parser Char
item = Parser $ ¥s ->
  case s of
  []      -> []
  (c:cs)  -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ ¥s -> concatMap (¥(a, s') -> parse (f a) s') $ parse p s

