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
