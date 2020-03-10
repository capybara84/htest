module Lexer where

data TokenType = TOK_INT | TOK_IDENT | TOK_KEYWORD
    deriving (Show)
data Token = Token { tokenType :: TokenType, valueString :: String }
instance Show Token where
    show (Token _ str) = show str

c_keywords = ["if","let"]

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x `elem` ['_','a'..'z'] ++ ['A'..'Z'] = lexParseText [x] xs
    | x `elem` ['0'..'9'] = lexParseNum [x] xs
    | otherwise = lexer xs

lexParseText :: String -> String -> [Token]
lexParseText _ [] = error "Lexer couldn't parse identifier"
lexParseText tokStr (x:xs)
    | x `elem` ['_','a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        = lexParseText (tokStr ++ [x]) xs
    | tokStr `elem` c_keywords = (Token TOK_KEYWORD tokStr):lexer (x:xs)
    | otherwise = (Token TOK_IDENT tokStr):lexer (x:xs)

lexParseNum :: String -> String -> [Token]
lexParseNum _ [] = error "Lexer couldn't parse numric literal"
lexParseNum tokStr (x:xs)
    | x `elem` ['0'..'9'] = lexParseNum (tokStr ++ [x]) xs
    | otherwise = (Token TOK_INT tokStr):lexer (x:xs)
