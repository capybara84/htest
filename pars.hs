module Main where

import Data.Char
import Data.List

{-
expr = term ("+"|"-") expr | term
term = factor ("*"|"/") term | factor
factor = "(" expr ")" | nat
nat = ('0'..'9')+
-}


data Token
    = PlusTok
    | MinusTok
    | TimesTok
    | DivTok
    | OpenTok
    | CloseTok
    | IntTok Int
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('/' : restStr) = DivTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer (chr : restStr) | isSpace chr = lexer restStr
lexer str@(chr : _)
    | isDigit chr
    = IntTok (stringToInt digitStr) : lexer restStr
    where
        (digitStr, restStr) = break (not . isDigit) str
        stringToInt :: String -> Int
        stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
lexer (chr : restStr) = error $ "lexer: unexpected character: '" ++ show chr ++ "'"

data Expr
    = IntLit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

{-
expr = term {("+"|"-") term}
term = factor {("*"|"/") factor}
factor = "(" expr ")" | INT
-}
parse_expr :: [Token] -> (Expr, [Token])
parse_expr tokens =
    let (lhs, restTokens) = parse_term tokens in
    if restTokens == [] then
        (lhs, [])
    else
        let op = head restTokens in
        if op == PlusTok || op == MinusTok then
            let (rhs, restTokens') = parse_term $ tail restTokens in
            case op of
                PlusTok -> (Add lhs rhs, restTokens')
                MinusTok -> (Sub lhs rhs, restTokens')
        else
            (lhs, restTokens)
parse_term :: [Token] -> (Expr, [Token])
parse_term tokens =
    let (lhs, restTokens) = parse_factor tokens in
    if restTokens == [] then
        (lhs, [])
    else
        let op = head restTokens in
        if op == TimesTok || op == DivTok then
            let (rhs, restTokens') = parse_term $ tail restTokens in
            case op of
                TimesTok -> (Mul lhs rhs, restTokens')
                DivTok -> (Div lhs rhs, restTokens')
        else
            (lhs, restTokens)
parse_factor :: [Token] -> (Expr, [Token])
parse_factor tokens =
    case head tokens of
    OpenTok ->
        let (e, restTokens) = parse_expr $ tail tokens in
        if head restTokens /= CloseTok then
            error "missing ')'"
        else
            (e, tail restTokens)
    IntTok n ->
        (IntLit n, tail tokens)
    _ -> error "syntax error"

parse :: [Token] -> Expr
parse tokens =
    let (e, _) = parse_expr tokens in
    e

main :: IO ()
main = do
    print $ parse $ lexer "(1 + 2) * 3"
    return ()
