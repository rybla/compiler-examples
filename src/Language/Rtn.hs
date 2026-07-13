module Language.Rtn where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text.Lazy (LazyText)
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pp
import Prettyprinter.Util qualified as Pp
import Text.Parsec (ParseError, SourceName, SourcePos, char, choice, getPosition, many, runParser)
import Text.Parsec.Language
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token qualified as Token
import Utilities.Unsafe

--------------------------------
-- Syntax
--------------------------------

data Term ann = Node ann Text [Term ann]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pattern Leaf :: ann -> Text -> Term ann
pattern Leaf ann l = Node ann l []

instance Pretty (Term ann) where
  pretty (Leaf _ l) = pretty l
  pretty (Node _ l ts) = "(" <> pretty l <> (Pp.vsep . fmap pretty) ts <> ")"

--------------------------------
-- Parsing
--------------------------------

type Ann = (SourcePos, SourcePos)

-- | A label can contain any characters except whitespace and parentheses.
parseLabel :: Parser Text
parseLabel = todo "parse a label"

parseWhiteSpaces :: Parser ()
parseWhiteSpaces = todo "parse any amount of white-space"

parseTerm :: Parser (Term Ann)
parseTerm =
  choice
    [ do
        p0 <- getPosition
        (void . char) '('
        l <- parseLabel
        ts <- many parseTerm
        (void . char) ')'
        p1 <- getPosition
        parseWhiteSpaces
        pure $ Node (p0, p1) l ts,
      do
        p0 <- getPosition
        l <- parseLabel
        p1 <- getPosition
        parseWhiteSpaces
        pure $ Leaf (p0, p1) l
    ]

runParseTerm :: SourceName -> LazyText -> Either ParseError (Term Ann)
runParseTerm = runParser parseTerm ()

--------------------------------
-- RtnParser
--------------------------------
