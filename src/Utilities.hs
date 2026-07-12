{- HLINT ignore "Avoid restricted function" -}

module Utilities where

import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), SimpleDocStream, layoutPretty)

layoutUnbounded :: Doc ann -> SimpleDocStream ann
layoutUnbounded = layoutPretty (LayoutOptions Unbounded)

safeDiv :: (Integral a) => a -> a -> Maybe a
safeDiv n d = if d == 0 then Nothing else Just (n `div` d)
