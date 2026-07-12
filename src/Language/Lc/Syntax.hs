module Language.Lc.Syntax where

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty (pretty), parens, (<+>))
import Prettyprinter.Util (reflow)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Arbitrary (Arbitrary (shrink))
import Utilities.Unsafe

--------------------------------

data Tm
  = -- | Literal string value
    Lit Text
  | -- | DeBruijn-indexed variable reference
    Var Natural
  | -- | λ-abstraction
    Lam Tm
  | -- | function application
    App Tm Tm
  deriving (Generic, Eq, Show, Ord)

instance Pretty Tm where
  pretty (Lit t) = reflow $ Text.show t
  pretty (Var i) = "@" <> pretty i
  pretty (Lam b) = parens $ "λ" <+> pretty b
  pretty (App f a) = parens $ pretty f <+> pretty a

-- | Arbitrary *closed* terms.
instance Arbitrary Tm where
  arbitrary = todo "arbitrary generator of terms"

  shrink = todo "shrink a term for property-based testing"
