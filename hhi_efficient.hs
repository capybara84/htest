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
interp p = fromJust (lookup "main" bs)
  where
    bs = prims ++ map (\(f, e) -> (f, link bs e)) p

link bs (Ap f a) = link bs f ! link bs a
link bs (Fun f) = fromJust (lookup f bs)
link bs e = e

infixl 0 !
(Lam f) ! x = f x

prims = let (-->) = (,) in
  [ "I" --> (Lam $ \x -> x)
  , "K" --> (Lam $ \x -> Lam $ \y -> x)
  , "S" --> (Lam $ \f -> Lam $ \g -> Lam $ \x -> f!x!(g!x))
  , "B" --> (Lam $ \f -> Lam $ \g -> Lam $ \x -> f!(g!x))
  , "C" --> (Lam $ \f -> Lam $ \g -> Lam $ \x -> f!x!g)
  , "ADD_W" --> arith2 (+), "SUB_W" --> arith2 (-)
  , "EQ_W" --> logical2 (==), "NE_W" --> logical2 (/=)
  , "LE_W" --> logical2 (<=)
  ]

arith2 op = Lam $ \(Int a) -> Lam $ \(Int b) -> Int (op a b)

logical2 op =
  Lam $ \(Int a) -> Lam $ \(Int b) -> if op a b then true else false

true = Lam $ \t -> Lam $ \f -> t
false = Lam $ \t -> Lam $ \f -> f

