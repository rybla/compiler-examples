module Language.Calculator.Interpretation where

import Language.Calculator.Syntax

--------------------------------

interpret :: Tm -> Integer
interpret (Literal v) = v
interpret (Operation Plus t1 t2) = interpret t1 + interpret t2
interpret (Operation Times t1 t2) = interpret t1 * interpret t2
