module Language.Calculator.Syntax where

import GHC.Generics (Generic)

data Tm ann
  = Literal ann Integer
  | Operation ann Operator (Tm ann) (Tm ann)
  deriving (Generic, Eq, Show, Ord)

data Operator
  = Plus
  | Times
  deriving (Generic, Eq, Show, Ord)
