-- https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf
-- Evaluating Haskell in Haskell
import Data.Maybe
import Text.Show.Functions

type Prog = [(FunId, Exp)]

type VarId = String

type FunId = String

data Exp = Ap Exp Exp
         | Lambda VarId Exp
         | Fun FunId
         | Var VarId
         | Int Int
         | Lam (Exp -> Exp)
    deriving Show

interp :: Prog -> Exp
interp p = eval p main
  where
    main = fromJust (lookup "main" p)

eval :: Prog -> Exp -> Exp
eval p (Ap (Ap (Fun "ADD_W") a) b) = arith2 p (+) a b
eval p (Ap (Ap (Fun "SUB_W") a) b) = arith2 p (-) a b
eval p (Ap (Ap (Fun "EQ_W") a) b) = logical2 p (==) a b
eval p (Ap (Ap (Fun "NE_W") a) b) = logical2 p (/=) a b
eval p (Ap (Ap (Fun "LE_W") a) b) = logical2 p (<=) a b
eval p (Ap f a) = eval p (subst v a b)
  where
    Lambda v b = eval p f
eval p (Fun f) = eval p (fromJust (lookup f p))
eval p e = e

subst :: VarId -> Exp -> Exp -> Exp
subst v e (Var w) = if v == w then e else Var w
subst v e (Ap e0 e1) = Ap (subst v e e0) (subst v e e1)
subst v e (Lambda x b) = Lambda x (if v == x then b else subst v e b)
subst v e b = b

arith2 p op a b = Int (op x y)
  where
    Int x = eval p a
    Int y = eval p b

logical2 p op a b = if op x y then true else false
  where
    Int x = eval p a
    Int y = eval p b

true = Lambda "t" $ Lambda "f" $ Var "t"
false = Lambda "t" $ Lambda "f" $ Var "f"


