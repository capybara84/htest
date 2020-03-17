

data Exp
  = EInt Int
  | EVar String
  | ESub Exp Exp
  | EIf Exp Exp Exp Exp
  | EFun String Exp
  | EApp Exp Exp
  deriving (Show)

type Env = [(String, Val)]

data Val
  = VInt Int
  | VClosure String Exp Env

instance Show Val where
  show (VInt i) = show i
  show VClosure{} = "<closure>"

eval :: Exp -> Env -> Val
eval (EInt i) env = VInt i
eval (EVar s) env =
  case lookup s env of
    Just (VInt n) -> VInt n
    Just (VClosure s e v) -> VClosure s e v
    Nothing -> error "missing"
eval (ESub e1 e2) env =
  let VInt v1 = eval e1 env in
  let VInt v2 = eval e2 env in
  VInt (v1 - v2)
eval (EIf e1 e2 e3 e4) env =
  let VInt v1 = eval e1 env in
  let VInt v2 = eval e2 env in
  if v1 <= v2 then
    eval e3 env
  else
    eval e4 env
eval (EFun x e) env = VClosure x e env
eval (EApp e1 e2) env =
  let VClosure x e env' = eval e1 env in
  let v = eval e2 env in
  let env'' = (x, v) : env' in
  eval e env''

one_plus_two = ESub (EInt 1) (ESub (EInt 0) (EInt 2) )

_Let x e1 e2 =
  EApp (EFun x e2) e1

abst = _Let "abs"
  (EFun "x" (EIf (EVar "x") (EInt 0)
                 (ESub (EInt 0) (EVar "x"))
                 (EVar "x")))
  (EApp (EVar "abs") (EInt (-42)))

{-
eval one_plus_two []
eval abst []
-}
