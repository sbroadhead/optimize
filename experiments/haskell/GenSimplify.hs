module GenSimplify where

import GenExpr

trySimplify :: Expr -> (Bool, Expr)

simplifyIters = 1000
simplify expr = simplify' expr 0
  where
    simplify' expr n
        | n == simplifyIters = expr
        | otherwise =
            let (changed, expr') = trySimplify expr in
            if changed
                then simplify' expr' (n+1)
                else expr'

exprSort (Add a b) = if a < b then Add a b else Add b a 
exprSort (Sub a b) = if a < b then Sub a b else Sub b a
exprSort (Mul a b) = if a < b then Mul a b else Mul b a
exprSort (Div a b) = if a < b then Div a b else Div b a
exprSort x = x

-- Collecting like terms in addition
tryCollect a b 
    | a == b = (True, Mul (Const 2.0) a)
    | otherwise = (False, Const 0.0)

-- Arithmetic
trySimplify (Add (Const a) (Const b)) = (True, Const $ a + b)
trySimplify (Sub (Const a) (Const b)) = (True, Const $ a - b)
trySimplify (Mul (Const a) (Const b)) = (True, Const $ a * b)
trySimplify (Div (Const a) (Const b)) = (True, Const $ a / b)
trySimplify (Sin (Const a)) = (True, Const $ sin a)
trySimplify (Cos (Const a)) = (True, Const $ cos a)
trySimplify (Log (Const a)) = (True, Const $ log a)
trySimplify (Negate (Const a)) = (True, Const $ (-a))
-- Additive identity
trySimplify (Add (Const 0.0) e) = (True, exprSort e)
trySimplify (Add e (Const 0.0)) = (True, exprSort e)
-- Multiplicative identity
trySimplify (Mul (Const 1.0) e) = (True, exprSort e)
trySimplify (Mul e (Const 1.0)) = (True, exprSort e)
trySimplify (Div e (Const 1.0)) = (True, exprSort e)
-- Multiplicative inverse
trySimplify (Mul (Const 0.0) e) = (True, Const 0.0)
trySimplify (Mul e (Const 0.0)) = (True, Const 0.0)
-- Recursive simplification
trySimplify (Add a b) =
    let (l, a') = trySimplify $ exprSort a
        (r, b') = trySimplify $ exprSort b
        (s, c') = tryCollect a' b'
    in (l || r || s, if s then c' else Add a' b')
trySimplify (Sub a b) =
    let (l, a') = trySimplify $ exprSort a
        (r, b') = trySimplify $ exprSort b
    in (l || r, Sub a' b')
trySimplify (Mul a b) =
    let (l, a') = trySimplify $ exprSort a
        (r, b') = trySimplify $ exprSort b
    in (l || r, Mul a' b')
trySimplify (Div a b) =
    let (l, a') = trySimplify $ exprSort a
        (r, b') = trySimplify $ exprSort b
    in (l || r, Div a' b')
trySimplify (Sin a) =
    let (x, a') = trySimplify $ exprSort a
    in (x, Sin a')
trySimplify (Cos a) =
    let (x, a') = trySimplify $ exprSort a
    in (x, Cos a')
trySimplify (Log a) =
    let (x, a') = trySimplify $ exprSort a
    in (x, Log a')

-- No simplification
trySimplify e = (False, e)
