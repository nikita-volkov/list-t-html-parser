module ListT.HTMLParser.XML where

import ListT.HTMLParser.Prelude
import qualified Data.XML.Types as XML
import qualified HTMLTokenizer as Tokenizer


type Parser a =
  StateT [Tokenizer.Token] Maybe a

run :: Parser a -> [Tokenizer.Token] -> Maybe a
run =
  evalStateT

fetchToken :: Parser Tokenizer.Token
fetchToken =
  StateT uncons

tryEOI :: Parser Bool
tryEOI =
  fmap null get

closingTag :: Tokenizer.Identifier -> Parser ()
closingTag ident =
  fetchToken >>= \case
    Tokenizer.Token_ClosingTag ident' | ident' == ident -> return ()
    _ -> mzero

-- |
-- Expects the input tokens to be proper.
node :: Parser XML.Node
node =
  fetchToken >>= \case
    Tokenizer.Token_Text x -> return (XML.NodeContent (XML.ContentEntity x))
    Tokenizer.Token_Comment x -> return (XML.NodeComment x)
    Tokenizer.Token_OpeningTag (ident, attrs, closed) -> do
      subnodes <- if closed
        then return []
        else many node <* closingTag ident
      return (XML.NodeElement (XML.Element (convertIdent ident) (fmap convertAttribute attrs) subnodes))
    _ -> mzero
  where
    convertIdent (Tokenizer.Identifier namespace name) = XML.Name (convert name) (fmap convert namespace) Nothing
    convertAttribute (ident, content) = (convertIdent ident, convert (fmap XML.ContentEntity content))
