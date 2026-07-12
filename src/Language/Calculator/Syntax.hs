-- {- HLINT ignore "Avoid restricted function" -}

module Language.Calculator.Syntax where

import Control.Lens hiding (elements, op)
import GHC.Generics (Generic)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary, shrink), elements, oneof, sized)
import Utilities (safeDiv)

--------------------------------

data Tm
  = Literal Int
  | Operation Operator Tm Tm
  deriving (Generic, Eq, Show, Ord)

data Operator
  = Plus
  | Times
  deriving (Generic, Eq, Show, Ord)

instance Arbitrary Operator where
  arbitrary = elements [Plus, Times]

instance Arbitrary Tm where
  arbitrary = sized genTm
    where
      genTm 0 = Literal <$> arbitrary
      genTm n =
        oneof
          [ Literal <$> arbitrary,
            do
              a <- n `safeDiv` 2 & maybe (genTm 0) genTm
              b <- n `safeDiv` 2 & maybe (genTm 0) genTm
              (\op -> Operation op a b) <$> arbitrary
          ]

  shrink (Literal n) = [Literal n' | n' <- shrink n]
  shrink (Operation op t1 t2) =
    concat
      [ [t1, t2],
        [Operation op' t1 t2 | op' <- shrink op],
        [Operation op t1' t2 | t1' <- shrink t1],
        [Operation op t1 t2' | t2' <- shrink t2]
      ]
