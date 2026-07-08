module Language.Calculator.Syntax where

import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary))
import Utilities.Unsafe (todo)

--------------------------------

data Tm
  = Literal Int
  | Operation Operator Tm Tm
  deriving (Generic, Eq, Show, Ord)

data Operator
  = Plus
  | Times
  deriving (Generic, Eq, Show, Ord)

instance Arbitrary Tm where
  arbitrary = todo "generate arbitrary calculator term"
