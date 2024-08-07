import ExprT
import Parser ( parseExp )

-- Exercise 1
eval :: ExprT -> Integer
eval (Mul expr1 expr2) = eval expr1 * eval expr2
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Lit lit) = lit

-- Exercise 2
safeEval :: Maybe ExprT -> Maybe Integer
safeEval (Just e) = Just (eval e)
safeEval Nothing = Nothing

evalStr :: String -> Maybe Integer
evalStr s = safeEval (parseExp Lit Add Mul s)

-- Exercise 3
class Expr a where
  lit :: a -> ExprT
  mul :: a -> a -> ExprT
  add :: a -> a -> ExprT

instance Expr ExprT where
  lit = id
  mul = Mul
  add = Add

reify :: ExprT -> ExprT
reify = id