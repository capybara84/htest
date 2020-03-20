module Main where

data ParserContext = PC { pc_input :: String }

initPC :: String -> ParserContext
initPC s = PC { pc_input = s }

printPC :: ParserContext -> IO ()
printPC pc = do
    print $ pc_input pc
    return ()

next :: ParserContext -> ParserContext
next pc = PC { pc_input = tail $ pc_input pc }

type Expression = ParserContext -> (Bool, ParserContext)

ch :: Char -> Expression
ch c pc =
    if pc_input pc /= [] && head (pc_input pc) == c
    then (True, next pc)
    else (False, pc)

sequ :: Expression -> Expression -> Expression
sequ e1 e2 pc =
    let (res1, pc1) = e1 pc in
    if res1
    then e2 pc1
    else (False, pc)

ord :: Expression -> Expression -> Expression
ord e1 e2 pc =
    let (res1, pc1) = e1 pc in
    if res1
    then (True, pc1)
    else e2 pc

many :: Expression -> Expression
many e pc =
    let (res, pc1) = e pc in
    if res
    then many e pc1
    else (True, pc)

abc = sequ (ch 'a') $ sequ (ch 'b') (ch 'c')
a_or_b = ord (ch 'a') (ch 'b')

num = ord (ch '0') $ ord (ch '1') $ ord (ch '2') $ ord (ch '3') $ ord (ch '4')
    $ ord (ch '5') $ ord (ch '6') $ ord (ch '7') $ ord (ch '8') (ch '9')
value = sequ num (many num)
prod = sequ value $ many (sequ (ord (ch '*') (ch '/')) value)
expression = sequ prod $ many (sequ (ord (ch '+') (ch '0')) prod)

test :: Expression -> String -> IO ()
test e s = do
    let (_, pc) = e $ initPC s
    printPC pc
    return ()


main :: IO ()
main = do
    test (ch 'a') "abcba"
    test abc "abcba"
    test a_or_b "bcba"
    test (many (ch 'a')) "aaaaa"
    test expression "1+2*3"
    test expression "12+3*4"
    return ()


{-
main :: IO ()
main = do
    let pc = initPC "aabcd"
    printPC pc
    let pc' = next pc
    printPC pc'
    return ()
-}
