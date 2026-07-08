module Language.Rtn.Syntax where

import Data.Text (Text)
import Data.Tree (Tree (Node))
import Data.Tree qualified as Tree
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pp
import Prettyprinter.Util qualified as Pp

--------------------------------

type Tm ann = Tree.Tree (Label ann)

pattern Leaf :: Label ann -> Tree.Tree (Label ann)
pattern Leaf l = Tree.Node l []

instance {-# OVERLAPPING #-} Pretty (Tm ann) where
  pretty (Leaf l) = pretty l
  pretty (Node l ts) = "(" <> pretty l <> (Pp.vsep . fmap pretty) ts <> ")"

data Label ann = Label
  { labelAnn :: ann,
    labelValue :: Text
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Pretty (Label ann) where
  pretty l = Pp.reflow l.labelValue
