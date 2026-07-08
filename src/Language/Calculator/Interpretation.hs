module Language.Calculator.Interpretation where

import Language.Calculator.Syntax

interpret :: Tm ann -> Integer
interpret (Literal _ v) = v
interpret (Operation _ Plus t1 t2) = interpret t1 + interpret t2
interpret (Operation _ Times t1 t2) = interpret t1 * interpret t2
