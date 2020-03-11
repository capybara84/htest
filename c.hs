-- https://kseo.github.io/posts/2017-01-05-implementing-a-call-by-value-interpreter-in-haskell.html

data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  | Bot
  deriving Show

data Value
  = VInt Int
  | VClosure Expr Env

instance Show Value where
  show (VInt i) = show i
  show VClosure{} = "<closure>"

data PrimOp = Add | Mul
  deriving Show

type Env = [Value]

eval :: Env -> Expr -> Value
eval env (Var n) = env !! n
eval env (Lam a) = VClosure a env
eval env (App a b) =
  let VClosure c env' = eval env a in
  let v = eval env b in
  eval (v : env') c
eval env (Lit n) = VInt n
eval env (Prim p a b) =
  (evalPrim p) (eval env a) (eval env b)
eval env Bot = error "Evaluation would not terminate"

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)

emptyEnv :: Env
emptyEnv = []

-- (\x y -> x) x bot
test :: Value
test = eval emptyEnv $ App (App (Lam (Lam (Var 1))) (Lit 10)) Bot

