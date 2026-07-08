module Utilities where

import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), SimpleDocStream, layoutPretty)

layoutUnbounded :: Doc ann -> SimpleDocStream ann
layoutUnbounded = layoutPretty (LayoutOptions Unbounded)
