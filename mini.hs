module Main where

import qualified Data.Map as Map

data Exp
    = EInt Int
    | EVar String
    | EFun String Exp
    | EApp Exp Exp
    | EBinExp BinOp Exp Exp
    deriving Show

data BinOp = Add | Sub | Mul | Div
    deriving Show

data Val
    = VInt Int
    | VClosure String Exp Env

instance Show Val where
    show (VInt i) = show i
    show VClosure {} = "<closure>"

type Env = Map.Map String Val

eval :: Env -> Exp -> (Val, Env)
eval env (EInt n) = (VInt n, env)
eval env (EVar var) = 
    case Map.lookup var env of
    Nothing -> error $ "no such variable: " ++ var
    Just x -> (x, env)
eval env (EFun param body) = (VClosure param body env, env)
eval env (EApp fn arg) =
    let (VClosure param body _, env') = eval env fn in
    let (val, _) = eval env arg in
    let new_env = Map.insert param val env'  in
    let (v, _) = eval new_env body in
    (v, env)
eval env (EBinExp op lhs rhs) =
    (evalOp op lhv rhv, env)
    where
        (lhv, _) = eval env lhs
        (rhv, _) = eval env rhs

evalOp :: BinOp -> Val -> Val -> Val
evalOp Add (VInt a) (VInt b) = VInt (a + b)
evalOp Sub (VInt a) (VInt b) = VInt (a - b)
evalOp Mul (VInt a) (VInt b) = VInt (a * b)
evalOp Div (VInt a) (VInt b) = VInt (a `div` b)

-- (\x -> x + 1) 3
test :: Exp
test =
    EApp (EFun "x" (EBinExp Add (EVar "x") (EInt 1))) (EInt 3)

main :: IO ()
main = do
    print $ fst $ eval Map.empty test
    return ()
