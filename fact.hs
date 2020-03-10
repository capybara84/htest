-- $ stack repl fact.hs
-- > fact 5

fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n-1)
