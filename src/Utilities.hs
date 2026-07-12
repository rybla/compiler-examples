{- HLINT ignore "Avoid restricted function" -}
{- HLINT ignore "Avoid partial function" -}

module Utilities where

import Control.Monad.Error.Class (MonadError (throwError))
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), SimpleDocStream, layoutPretty)

layoutUnbounded :: Doc ann -> SimpleDocStream ann
layoutUnbounded = layoutPretty (LayoutOptions Unbounded)

-- | Safely divide an `Integral`.
safeDiv :: (Integral a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv n d = Just $ n `div` d

-- | Safely halve an `Integral`.
halve :: (Integral a) => a -> a
halve n = n `div` 2

justOrThrow :: (MonadError e m) => e -> Maybe a -> m a
justOrThrow _ (Just a) = pure a
justOrThrow e Nothing = throwError e
