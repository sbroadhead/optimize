{-# LANGUAGE RebindableSyntax #-}

module GenTest where

import GenObjective
-- This makes numeric literals default to Integer so we don't have to use type annotations
import Prelude hiding (Num(fromInteger))

fromInteger :: Integer -> Integer
fromInteger = id

opt1 = do
    x <- array 5
    y <- array 5
    z <- var

    x?0 $= con 0.0
    x?(i `for` [1..4]) $= (x?(i#-#1)) .+. con 1.0

    minimize $ sum' i [0..5] ((x?i) .-. (y?i))

-- TODO: topsort the variable declarations, do substitutions to get the final objective
-- generate C++ code for objective, generate and simplify the gradient and generate C++ for that
-- allow constraints and convert to barriers
