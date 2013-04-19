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

exprSort (Add (Const a) (Const b)) = if a < b then Add (Const a) (Const b) else Add (Const b) (Const a)
exprSort (Add (Const a) b) = Add (Const a) (exprSort b)
exprSort (Add a (Const b)) = Add (Const b) (exprSort a)
exprSort (Add (Var a) (Var b)) = if a < b then Add (Var a) (Var b) else Add (Var a) (Var b)

exprSort (Mul (Const a) (Const b)) = if a < b then Mul (Const a) (Const b) else Mul (Const b) (Const a)
exprSort (Mul (Const a) b) = Mul (Const a) (exprSort b)
exprSort (Mul a (Const b)) = Mul (Const b) (exprSort a)
exprSort (Mul (Var a) (Var b)) = if a < b then Mul (Var a) (Var b) else Mul (Var a) (Var b)
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
trySimplify (Sub (Const 0.0) e) = (True, Negate $ exprSort e)
trySimplify (Sub e (Const 0.0)) = (True, exprSort e)
-- Multiplicative identity
trySimplify (Mul (Const 1.0) e) = (True, exprSort e)
trySimplify (Mul e (Const 1.0)) = (True, exprSort e)
trySimplify (Div e (Const 1.0)) = (True, exprSort e)
-- Multiplicative zero
trySimplify (Mul (Const 0.0) e) = (True, Const 0.0)
trySimplify (Mul e (Const 0.0)) = (True, Const 0.0)
-- Reassociate multiplication to the left
trySimplify (Mul a (Mul b c)) = (True, Mul (Mul a b) c)
-- Recursive simplification
trySimplify (Add a b) =
    let (p, q) =
            -- Reassociate addition to the left
            case (a, b) of
                (Add x y, z) -> (a, b)
                (x, Add y z) -> (Add x y, z)
                _ -> (a, b)
        (l, a') = trySimplify $ exprSort p
        (r, b') = trySimplify $ exprSort q
        (l', a'') = trySimplify $ exprSort a
        (r', b'') = trySimplify $ exprSort b
        (s, c') = tryCollect a'' b'' --try collecting before reassociating
    in (l || r || s, if s then c' else Add a' b')
trySimplify (Sub a b) =
    let (l, a') = trySimplify $ exprSort a
        (r, b') = trySimplify $ exprSort b
    in (l || r, Sub a' b')
trySimplify (Mul a b) =
    let (p, q) =
            -- Reassociate multiplication to the left
            case (a, b) of
                (Mul x y, z) -> (a, b)
                (x, Mul y z) -> (Mul x y, z)
                _ -> (a, b)
        (l, a') = trySimplify $ exprSort p
        (r, b') = trySimplify $ exprSort q
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
