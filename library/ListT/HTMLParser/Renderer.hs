module ListT.HTMLParser.Renderer where

import BasePrelude hiding (fromString)
import Conversion
import Conversion.Text
import Data.Text (Text)
import Data.Text.Lazy.Builder
import HTMLTokenizer (Token(..), OpeningTag, Identifier(..), Attribute)
import qualified Data.CaseInsensitive as CI


openingTag :: OpeningTag -> Builder
openingTag (name, attrs, closed) =
  singleton '<' <>
  identifier name <>
  mconcat (map (mappend (singleton ' ') . attribute) attrs) <>
  bool (singleton '>') (fromString "/>") closed

attribute :: Attribute -> Builder
attribute (name, value) =
  maybe id (flip mappend . mappend (fromString "=\"") . flip mappend (singleton '"') . convert) value $
  identifier name

identifier :: Identifier -> Builder
identifier (Identifier namespace name) =
  foldMap (flip mappend ":" . convert . CI.foldedCase) namespace <> (convert . CI.foldedCase) name

closingTag :: Identifier -> Builder
closingTag name =
  fromString "</" <> identifier name <> singleton '>'

text :: Text -> Builder
text =
  convert

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
