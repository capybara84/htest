-- https://wiki.haskell.org/wikiupload/0/0a/TMR-Issue10.pdf
-- Evaluating Haskell in Haskell

type Prog = [(FunId, Exp)]

type VarId = String

type FunId = String

data Exp = Ap Exp Exp
         | Lambda VarId Exp
         | Fun FunId
         | Var VarId
         | Int Int
         | Lam (Exp -> Exp)

{--
data List a = Nil | Cons a (List a)

powerset xs =
    case xs of
        Nil -> Cons Nil Nil
        Cons x xs -> let p = powerset xs in
                        p ++ map (Cons x) p
--}

Nil = \n -> \c -> n

Cons = \x -> \xs -> \n -> \c -> c x xs


powerset =
    \xs -> xs (Cons Nil Nil)
              (\x xs -> (\p -> p ++ map (cons x) p) (powerset xs))

