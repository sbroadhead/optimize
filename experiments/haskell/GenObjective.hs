{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module GenObjective where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

data OptState
    = MkOptState
    { os_varcount :: Integer
    , os_objective :: Expr
    , os_vars :: Map Integer Expr
    }
    deriving (Show, Eq, Ord)

newtype Opt a = Opt (State OptState a)
    deriving (Monad, MonadState OptState)

data BoundVar = BoundVar String Integer
    deriving (Show, Eq, Ord)
(BoundVar s i) #-# n = BoundVar s (i-n)
(BoundVar s i) #+# n = BoundVar s (i+n)

i = BoundVar "i" 0

data Quantifier = Quantifier VarDef String [Integer]
    deriving (Show, Eq, Ord)

data QuantifierRange = QuantifierRange String [Integer]
    deriving (Show, Eq, Ord)
(BoundVar s _) `for` range = QuantifierRange s range

evalQuantifier :: String -> Integer -> Expr -> Expr
evalQuantifier s i (Add x y) = Add (evalQuantifier s i x) (evalQuantifier s i y)
evalQuantifier s i (Sub x y) = Sub (evalQuantifier s i x) (evalQuantifier s i y)
evalQuantifier s i (Mul x y) = Mul (evalQuantifier s i x) (evalQuantifier s i y)
evalQuantifier s i (Div x y) = Div (evalQuantifier s i x) (evalQuantifier s i y)
evalQuantifier s i (Sin x) = Sin (evalQuantifier s i x)
evalQuantifier s i (Cos x) = Cos (evalQuantifier s i x)
evalQuantifier s i (Negate x) = Negate (evalQuantifier s i x)
evalQuantifier s i (QuantifiedVar v@(VarDef x) (BoundVar s' off))
    | s == s' = Var (x+i+off)
    | otherwise = QuantifiedVar v (BoundVar s' off)
evalQuantifier s i (QuantifiedVar v@(ArrayDef n x) (BoundVar s' off))
    | s == s' = Var (n+i+off)
    | otherwise = QuantifiedVar v (BoundVar s' off)
evalQuantifier _ _ x = x

data VarDef
    = VarDef Integer
    | ArrayDef Integer Integer
    deriving (Show, Eq, Ord)

class Assignable a where
    ($=) :: a -> Expr -> Opt ()

instance Assignable Expr where
    (Var i) $= e = do
        vars <- gets os_vars
        case Map.lookup i vars of
            Just _ -> error $ "Variable already defined: " ++ show i
            Nothing -> modify $ \s -> s { os_vars = Map.insert i e vars }
    x $= _ = error $ "Trying to assign to a non-variable: " ++ show x

infixr 1 $=

instance Assignable Quantifier where
    (Quantifier (VarDef x) s range) $= _ = error "Quantifying over non-array on left hand side"
    (Quantifier (ArrayDef n x) s range) $= e = forM_ range $ \i -> (Var (n+i)) $= (evalQuantifier s i e)
        
class VarIndex a where
    type VarDeref a :: *
    deref :: VarDef -> a -> VarDeref a

instance VarIndex Integer where
    type VarDeref Integer = Expr
    deref (ArrayDef x n) i
        | i < 0 = error "Array index < 0"
        | i >= n = error $ "Array index >= " ++ show n
        | otherwise = Var (x+i)
    deref (VarDef x) i
        | i == 0 = Var x
        | otherwise = error "Dereferencing a non-array"

instance VarIndex QuantifierRange where
    type VarDeref QuantifierRange = Quantifier
    deref (ArrayDef x n) (QuantifierRange s range) = Quantifier (ArrayDef x n) s range
    deref _ _ = error "Quantifying over a non-array"

instance VarIndex BoundVar where
    type VarDeref BoundVar = Expr
    deref (ArrayDef x n) b = QuantifiedVar (ArrayDef x n) b
    deref _ _ = error "Dereferencing a non-array with a bound variable"

(?) :: VarIndex a => VarDef -> a -> VarDeref a
(?) = deref
infixr 4 ?

var :: Opt Expr
var = do
    varcount <- gets os_varcount
    modify $ \s -> s { os_varcount = varcount + 1 }
    return $ (VarDef varcount) ? (0::Integer)

array :: Integer -> Opt VarDef
array n = do
    varcount <- gets os_varcount
    modify $ \s -> s { os_varcount = varcount + n }
    return $ ArrayDef varcount n

sum' :: BoundVar -> [Integer] -> Expr -> Expr
sum' _ [] _ = con 0.0
sum' (BoundVar s _) range e = foldl1 Add $ map (\i -> evalQuantifier s i e) range

minimize :: Expr -> Opt ()
minimize e = do
    modify $ \s -> s { os_objective = e }

execOpt :: Opt a -> OptState
execOpt (Opt st) = execState st $ MkOptState 0 (con 0) Map.empty 

----

data Expr
    = Const Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Sin Expr
    | Cos Expr
    | Negate Expr
    | Var Integer
    | QuantifiedVar VarDef BoundVar
    deriving (Show, Eq, Ord)

con = Const
(.+.) = Add
(.-.) = Sub
(.*.) = Mul
(./.) = Div
sin' = Sin
cos' = Cos
negate' = Negate

infixl 6 .+., .-.
infixl 7 .*., ./.

----


