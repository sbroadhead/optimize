{-# LANGUAGE RebindableSyntax #-}

module GenTest where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import GenObjective
import GenExpr
import GenSimplify 
import GenC

-- This makes numeric literals default to Integer so we don't have to use type annotations
import Prelude hiding (Num(fromInteger))

fromInteger :: Integer -> Integer
fromInteger = id

opt1 = do
    l <- array 5
    x <- array 5
    y <- array 5
    z <- var

    x~~0 $= con 2.5
    y~~0 $= con 5.2
    x~~(i `for` [1..4]) $= (x~~(i @- 1)) .+. (l~~i)
    y~~(i `for` [1..4]) $= (y~~(i @- 1)) .*. (l~~i)

    (x~~i)%%(i `for` [1..4]) $<= (x~~(i @- 1))

    minimize $ sum' i [0..4] $ ((x~~i) .-. (y~~i)) .*. ((x~~i) .-. (y~~i))

opt' = execOpt opt1
objective = simplify $ substitute (os_vars opt') $ os_objective opt'
vars = varsOf objective
gradient = grad (Set.toList vars) objective

showObj = do
    putStrLn "Objective:"
    putStrLn $ "     f(x) = " ++ (pprint $ simplify $ objective)
    putStrLn "\n"
    putStrLn "Gradient:"
    forM_ (Map.assocs gradient) $ \(i, g) -> putStrLn $ "    d/dx[" ++ show i ++ "] =\n            " ++ (pprint $ simplify $ g)

-- generate C++ code for objective, generate and simplify the gradient and generate C++ for that
-- allow constraints and convert to barriers
