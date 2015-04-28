module ListT.HTMLParser.Renderer where

import BasePrelude hiding (fromString)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import HTMLTokenizer.Parser (Token(..), OpeningTag, ClosingTag, Attribute)
import qualified Data.CaseInsensitive as CI
import qualified HTMLEntities.Builder


openingTag :: OpeningTag -> Builder
openingTag (name, attrs, closed) =
  singleton '<' <>
  ciText name <>
  mconcat (intersperse (singleton ' ') (map attribute attrs)) <>
  bool (singleton '>') (fromString "/>") closed

attribute :: Attribute -> Builder
attribute (name, value) =
  maybe id (flip mappend . mappend (singleton '=') . HTMLEntities.Builder.text) value $
  ciText name

ciText :: CI.CI Text -> Builder
ciText =
  HTMLEntities.Builder.text . CI.foldedCase  

closingTag :: ClosingTag -> Builder
closingTag name =
  fromString "</" <> ciText name <> singleton '>'

text :: Text -> Builder
text =
  HTMLEntities.Builder.text

comment :: Text -> Builder
comment content =
  fromString "<!--" <> fromText content <> fromString "-->"

token :: Token -> Builder
token =
  \case
    Token_OpeningTag x -> openingTag x
    Token_ClosingTag x -> closingTag x
    Token_Text x -> text x
    Token_Comment x -> comment x
