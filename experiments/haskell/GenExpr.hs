module GenExpr where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

data Expr
    = Const Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Sin Expr
    | Cos Expr
    | Log Expr
    | Negate Expr
    | Var Integer
    | QuantifiedVar VarDef BoundVar
    deriving (Show, Eq, Ord)

data VarDef
    = VarDef Integer
    | ArrayDef Integer Integer
    deriving (Show, Eq, Ord)

data BoundVar = BoundVar String Integer
    deriving (Show, Eq, Ord)
(BoundVar s i) @- n = BoundVar s (i-n)
(BoundVar s i) @+ n = BoundVar s (i+n)

diff :: Integer -> Expr -> Expr
diff x (Const _) = Const 0.0
diff x (Add a b) = Add (diff x a) (diff x b)
diff x (Sub a b) = Sub (diff x a) (diff x b)
diff x (Mul a b) = Add (Mul (diff x a) b) (Mul a (diff x b))
diff x (Div a b) = Div (Sub (Mul (diff x a) b) (Mul a (diff x b))) (Mul b b)
diff x (Sin a) = Mul (diff x a) (Cos a)
diff x (Cos a) = Negate $ Mul (diff x a) (Sin a)
diff x (Log a) = Mul (diff x a) (Div (Const 1.0) a)
diff x (Negate a) = Negate $ diff x a
diff x (Var x')
    | x == x' = Const 1.0
    | otherwise = Const 0.0
diff x _ = error "Invalid node in expression differentiation"

grad :: [Integer] -> Expr -> Map Integer Expr
grad vars expr = Map.fromList [(i, diff i expr) | i <- vars]

varsOf (Var i) = Set.singleton i
varsOf (Add a b) = Set.union (varsOf a) (varsOf b)
varsOf (Sub a b) = Set.union (varsOf a) (varsOf b)
varsOf (Mul a b) = Set.union (varsOf a) (varsOf b)
varsOf (Div a b) = Set.union (varsOf a) (varsOf b)
varsOf (Sin a) = varsOf a
varsOf (Cos a) = varsOf a
varsOf (Log a) = varsOf a
varsOf _ = Set.empty

replaceIn i e (Var i') = if i' == i then e else (Var i')
replaceIn i e (Add a b) = Add (replaceIn i e a) (replaceIn i e b)
replaceIn i e (Sub a b) = Sub (replaceIn i e a) (replaceIn i e b)
replaceIn i e (Mul a b) = Mul (replaceIn i e a) (replaceIn i e b)
replaceIn i e (Div a b) = Div (replaceIn i e a) (replaceIn i e b)
replaceIn i e (Sin a) = Sin (replaceIn i e a)
replaceIn i e (Cos a) = Cos (replaceIn i e a)
replaceIn i e (Log a) = Log (replaceIn i e a)
replaceIn i e x = x

con = Const
(.+.) = Add
(.-.) = Sub
(.*.) = Mul
(./.) = Div
sin' = Sin
cos' = Cos
log' = Log
negate' = Negate

infixl 6 .+., .-.
infixl 7 .*., ./.

pprints :: Int -> Expr -> ShowS
pprints p (Const d)
    | d < 0 = pprints p (Negate $ Const (-d))
    | otherwise = shows d
pprints p (Add e1 e2) = showParen (p>6) $ pprints 6 e1 . showString "+" . pprints 7 e2
pprints p (Sub e1 e2) = showParen (p>6) $ pprints 6 e1 . showString "-" . pprints 7 e2
pprints p (Mul e1 e2) = showParen (p>7) $ pprints 7 e1 . showString "*" . pprints 8 e2
pprints p (Div e1 e2) = showParen (p>7) $ pprints 7 e1 . showString "/" . pprints 8 e2
pprints p (Sin e) = showString "sin" . showParen True (pprints 0 e)
pprints p (Cos e) = showString "cos" . showParen True (pprints 0 e)
pprints p (Log e) = showString "log" . showParen True (pprints 0 e)
pprints p (Negate e) = showParen (p>=6) $ showString "-" . pprints 8 e
pprints p (Var i) = showString "x[" . shows i . showString "]"
pprints p (QuantifiedVar v (BoundVar s i))
    | i < 0 = showString ("x[" ++ s ++ "-") . shows (-i) . showString "]"
    | i > 0 = showString ("x[" ++ s ++ "+") . shows i . showString "]"
    | otherwise = showString ("x[" ++ s ++ "]")

pprint :: Expr -> String
pprint e = pprints 0 e ""

