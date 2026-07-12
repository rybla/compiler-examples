module Language.Lc.Syntax where

import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty (pretty), parens, (<+>))

--------------------------------

data Tm
  = -- | DeBruijn-indexed variable reference
    Var Natural
  | -- | λ-abstraction
    Lam Tm
  | -- | function application
    App Tm Tm
  deriving (Generic, Eq, Show, Ord)

instance Pretty Tm where
  pretty (Var i) = "@" <> pretty i
  pretty (Lam b) = parens $ "λ" <+> pretty b
  pretty (App f a) = parens $ pretty f <+> pretty a
