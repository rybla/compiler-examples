module Language.Lc where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), put)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural, minusNaturalMaybe)
import Prettyprinter (Doc, Pretty (pretty), dquotes, parens, pretty, (<+>))
import Prettyprinter.Util (reflow)

--------------------------------
-- Syntax
--------------------------------

data Term
  = -- | Literal string value
    Lit Text
  | -- | DeBruijn-indexed variable reference
    Var Natural
  | -- | λ-abstraction
    Lam Term
  | -- | function application
    App Term Term
  deriving (Generic, Eq, Show, Ord)

instance Pretty Term where
  pretty (Lit t) = reflow $ Text.show t
  pretty (Var i) = "@" <> pretty i
  pretty (Lam b) = parens $ "λ" <+> pretty b
  pretty (App f a) = parens $ pretty f <+> pretty a

-- [TODO]: implement `instance Arbitrary Term`

--------------------------------
-- Interpretation
--------------------------------

normalize :: (MonadState Natural m, MonadError (Doc Void) m) => Term -> m Term
normalize (Lit t) = pure $ Lit t
normalize (Var i) = pure $ Var i
normalize (Lam b) = Lam <$> normalize b
normalize (App f a) = do
  do
    gas <- get
    case minusNaturalMaybe gas 1 of
      Nothing -> throwError $ "Attempted to normalize redex" <+> (dquotes . pretty) (App f a) <+> "when out of gas"
      Just gas' -> put gas'
  a' <- normalize a
  normalize f >>= \case
    Lam b -> pure $ subst 0 a' b
    _ -> throwError $ "Attempted to apply non-function" <+> (dquotes . pretty) f <+> "to" <+> (dquotes . pretty) a

subst :: Natural -> Term -> Term -> Term
subst _ _ (Lit t) = Lit t
subst i v (Var j) = case compare i j of
  LT -> Var j
  EQ -> v
  GT -> Var (j - 1)
subst i v (Lam b) = Lam (subst (succ i) v b)
subst i v (App f a) = App (subst i v f) (subst i v a)

--------------------------------
-- Compilation
--------------------------------

-- [TODO]: implement `instance CompileSki Term`
