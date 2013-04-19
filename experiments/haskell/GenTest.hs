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
    x <- var
    y <- var

    con 5.0 $<= x
    con 8.0 $<= y

    minimize $ x .+. y

opt2 = do
    a <- var
    b <- var
    c <- var
    x <- array 10
    y <- array 10

    let xs = [0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
    let ys = [0.22, 1.1, 3.7, 9.2, 15.5, 27.1, 37.7, 49.2, 64.7, 83.1]
    forM_ [0..9] $ \i -> x~~i $= con (xs !! (fromIntegral i))
    forM_ [0..9] $ \i -> y~~i $= con (ys !! (fromIntegral i))

    let t = a .+. (b .*. (x~~i)) .+. (c .*. (x~~i) .*. (x~~i))
    minimize $ sum' i [0..9] $ (t .-. (y~~i)) .*. (t .-. (y~~i))

    -- Solution according to Wolfram Alpha:
    --      a = -0.248270 b = 0.163021, c = 1.00468
    -- Solution according to generated code in L-BFGS solver:
    --     a = -0.248270  b = 0.163021, c = 1.004681

opt3 = do
    a <- var
    b <- var
    c <- var

    minimize $ a.*.a .+. b.*.b .+. con 6.0 .*. c .*. c .+. con 7.0

-- Banana function
opt4 = do
    x <- array 6

    let t1 = (con 1.0 .-. (x~~i))
    let t2 = ((x~~(i@+1)) .-. (x~~i).*.(x~~i))
    minimize $ sum' i [0..4] $ t1 .*. t1 .+. con 100.0 .*. t2 .*. t2

opt' = execOpt opt1
objective = simplify $ substitute (os_vars opt') $ os_objective opt'
vars = varsOf objective
gradient = Map.map simplify $ grad (Set.toList vars) objective

showObj = do
    putStrLn "Objective:"
    putStrLn $ "     f(x) = " ++ (pprint $ objective)
    putStrLn "\n"
    putStrLn "Gradient:"
    forM_ (Map.assocs gradient) $ \(i, g) -> putStrLn $ "    d/dx[" ++ show i ++ "] =\n            " ++ (pprint g)

showCode = putStr $ genCode objective gradient
