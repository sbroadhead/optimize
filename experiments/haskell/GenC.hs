{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenC where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State
import GenObjective
import GenExpr
import GenSimplify

data ExprGraph
    = ConstT Double
    | AddT Int Int
    | SubT Int Int
    | MulT Int Int
    | DivT Int Int
    | SinT Int
    | CosT Int
    | LogT Int
    | NegateT Int
    | VarT Integer
    deriving (Show, Eq, Ord)

data GenState
    = MkGenState
    { gs_exprs :: Map Expr Int
    , gs_graph :: IntMap ExprGraph
    , gs_free :: Int
    }
    deriving (Show, Eq, Ord)

newtype Gen a = Gen (State GenState a)
    deriving (Monad, MonadState GenState)

runGen :: Gen a -> (a, GenState)
runGen (Gen st) = runState st $ MkGenState Map.empty IntMap.empty 0

addOrGet :: Expr -> Gen Int
addOrGet expr = do
    exprs <- gets gs_exprs
    if expr `Map.member` exprs
        then return $ exprs Map.! expr
        else do { i <- gets gs_free
                ; modify $ \s -> s { gs_free = i+1, gs_exprs = Map.insert expr i (gs_exprs s) }
                ; return i
                }

makeGraphNode :: Int -> ExprGraph -> Gen ()
makeGraphNode i n = modify $ \s -> s { gs_graph = (IntMap.insert i n (gs_graph s)) }

makeGraph' :: Expr -> Gen Int
makeGraph' q@(Const d) = do { z <- addOrGet q; makeGraphNode z (ConstT d); return z }
makeGraph' q@(Add a b) = do { x <- makeGraph' a; y <- makeGraph' b; z <- addOrGet q; makeGraphNode z (AddT x y); return z }
makeGraph' q@(Sub a b) = do { x <- makeGraph' a; y <- makeGraph' b; z <- addOrGet q; makeGraphNode z (SubT x y); return z }
makeGraph' q@(Mul a b) = do { x <- makeGraph' a; y <- makeGraph' b; z <- addOrGet q; makeGraphNode z (MulT x y); return z }
makeGraph' q@(Div a b) = do { x <- makeGraph' a; y <- makeGraph' b; z <- addOrGet q; makeGraphNode z (DivT x y); return z }
makeGraph' q@(Sin a) = do { x <- makeGraph' a; z <- addOrGet q; makeGraphNode z (SinT x); return z }
makeGraph' q@(Cos a) = do { x <- makeGraph' a; z <- addOrGet q; makeGraphNode z (CosT x); return z }
makeGraph' q@(Log a) = do { x <- makeGraph' a; z <- addOrGet q; makeGraphNode z (LogT x); return z }
makeGraph' q@(Negate a) = do { x <- makeGraph' a; z <- addOrGet q; makeGraphNode z (NegateT x); return z }
makeGraph' q@(Var i) = do { z <- addOrGet q; makeGraphNode z (VarT i); return z }

makeGraph :: Expr -> (Int, IntMap ExprGraph)
makeGraph expr =
    let (i, s) = runGen (makeGraph' expr)
    in (i, gs_graph s)

ins :: ExprGraph -> [Int]
ins (AddT a b) = [a, b]
ins (SubT a b) = [a, b]
ins (MulT a b) = [a, b]
ins (DivT a b) = [a, b]
ins (SinT a) = [a]
ins (CosT a) = [a]
ins (LogT a) = [a]
ins _ = []

topSort :: IntMap ExprGraph -> [Int]
topSort exprs = topSort' exprs Set.empty []
  where
    topSort' exprs seen sorted =
        let leaves = IntMap.keys $ IntMap.filter (\x -> (Set.fromList $ ins x) `Set.isSubsetOf` seen) exprs
        in case null leaves of
            True ->
                if not $ IntMap.null exprs
                    then error "Cycle in expression graph during topological sort"
                    else sorted
            False ->
                let i = head leaves
                in topSort' (IntMap.delete i exprs) (Set.insert i seen) (sorted++[i])

varName i = "var" ++ show i

toC :: ExprGraph -> Map Integer Integer -> String
toC (ConstT d) _ = show d
toC (AddT a b) _ = varName a ++ "+" ++ varName b
toC (SubT a b) _ = varName a ++ "-" ++ varName b
toC (MulT a b) _ = varName a ++ "*" ++ varName b
toC (DivT a b) _ = varName a ++ "/" ++ varName b
toC (SinT a) _ = "sin(" ++ varName a ++ ")"
toC (CosT a) _ = "cos(" ++ varName a ++ ")"
toC (LogT a) _ = "log(" ++ varName a ++ ")"
toC (NegateT a) _ = "-" ++ varName a
toC (VarT i) vars = "x[" ++ (show $ vars Map.! i) ++ "]"

genDecls :: [Int] -> IntMap ExprGraph -> Map Integer Integer -> [String]
genDecls [] _ _ = []
genDecls (x:xs) exprs vars = ["double " ++ varName x ++ " = " ++ toC (exprs IntMap.! x) vars ++ ";"] ++ genDecls xs exprs vars

mapVars :: Expr -> Map Integer Integer
mapVars expr = Map.fromList $ zip (Set.toList $ varsOf expr) [0..]

genCode :: Expr -> Map Integer Expr -> String
genCode expr grad = unlines $ result
  where
    comp = do { objVar <- makeGraph' expr
              ; gradVars <- forM (Map.assocs grad) $ \(i, g) -> makeGraph' g >>= \x -> return (i, x)
              ; return (objVar, gradVars)
              }
    ((objVar, gradVars), st) = runGen comp
    sorted = topSort $ gs_graph st
    varMap = mapVars expr 
    decls = genDecls sorted (gs_graph st) varMap
    result =
        ["double obj_func(int n, const double *x, double *g) {"]
        ++ (map ("    "++) decls) ++
        [""]
        ++ (map (\(i, x) -> "    g[" ++ (show $ varMap Map.! i) ++ "] = " ++ varName x ++ ";") gradVars) ++
        [""
        ,"    return " ++ varName (objVar) ++ ";"
        ,"}"]
