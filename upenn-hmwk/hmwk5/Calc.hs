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
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  mul = Mul
  add = Add

-- Exercise 4
instance Expr Integer where
  lit x = x
  mul a b = a * b
  add a b = a + b

instance Expr Bool where
  lit x = if x <= 0 then False else True
  mul a b = a && b
  add a b = a || b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  mul (MinMax a) (MinMax b) = MinMax (maximum [a, b])
  add (MinMax a) (MinMax b) = MinMax (minimum [a, b])

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)

-- Exercise 6
class HasVars where
  var :: String -> a