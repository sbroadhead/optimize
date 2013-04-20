module GenOperators where

import Prelude hiding ((+), (-), (*), (/), negate, sin, cos, log, (.), sum)
import GenExpr
import GenObjective

(+) = (.+.)
(-) = (.-.)
(*) = (.*.)
(/) = (./.)
negate = negate'
sin = sin'
cos = cos'
log = log'
(.) :: VarIndex a => VarDef -> a -> VarDeref a
(.) = deref
sum = sum'
param = param'
