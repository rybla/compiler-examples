module Language.Lc.Interpretation where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), put)
import Data.Void (Void)
import Language.Lc.Syntax
import Numeric.Natural (Natural, minusNaturalMaybe)
import Prettyprinter (Doc, dquotes, pretty, (<+>))

--------------------------------

normalize :: (MonadState Natural m, MonadError (Doc Void) m) => Tm -> m Tm
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

subst :: Natural -> Tm -> Tm -> Tm
subst _ _ (Lit t) = Lit t
subst i v (Var j) = case compare i j of
  LT -> Var j
  EQ -> v
  GT -> Var (j - 1)
subst i v (Lam b) = Lam (subst (succ i) v b)
subst i v (App f a) = App (subst i v f) (subst i v a)
