module Language.Calculator.Syntax where

import GHC.Generics (Generic)

--------------------------------

data Tm
  = Literal Integer
  | Operation Operator Tm Tm
  deriving (Generic, Eq, Show, Ord)

data Operator
  = Plus
  | Times
  deriving (Generic, Eq, Show, Ord)
