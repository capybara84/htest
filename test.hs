-- $ stack runhaskell Test.hs
module Test where
x = 5
y = (6, "Hello")
z = x * fst y
main = putStrLn "Hello World"
