module Main where

import BasePrelude
import Test.Hspec
import Conversion
import Conversion.Text
import Data.Text (Text)
import qualified HTMLTokenizer.Parser
import qualified ListT.Attoparsec
import qualified ListT.HTMLParser
import qualified ListT.Text
import qualified ListT.HTMLParser as P


main =
  hspec $ do
    it "Backtracking" $ do
      let 
        text = "<a><b></b></a>"
        parser = 
          a <|> b
          where
            a = 
              do
                P.openingTag
                P.openingTag
                P.openingTag
                return 1
            b =
              do
                P.openingTag
                P.openingTag
                P.closingTag
                P.closingTag
                return 2
      result <- parse parser text
      shouldBe result (Right 2)
    it "Complex" $ do
      let 
        text = "<a><b><c></c></b></a>"
        parser = 
          a <|> b
          where
            a = 
              do
                P.openingTag
                b <|> c
                P.openingTag
                return 1
              where
                b =
                  do
                    ("b", _, _) <- P.openingTag
                    P.closingTag
                c =
                  do
                    ("b", _, _) <- P.openingTag
                    ("c", _, _) <- P.openingTag
                    P.closingTag
            b =
              do
                P.openingTag
                P.openingTag
                P.openingTag
                P.closingTag
                P.closingTag
                P.closingTag
                return 2
      result <- parse parser text
      shouldBe result (Right 2)



parse :: ListT.HTMLParser.Parser IO a -> Text -> IO (Either Error a)
parse parser =
  fmap (either (Left . parseSomeException) id) . try .
  fmap (either (Left . Error_Parsing) Right) .
  ListT.HTMLParser.run parser . ListT.Attoparsec.textParser HTMLTokenizer.Parser.token . 
  ListT.Text.stream 2
  where
    parseSomeException e =
      fromMaybe (error $ showString "Unexpected exception: " $ shows e $ "") $
        Error_Lexing <$> fromException e

data Error =
  -- | A tokenization failure
  Error_Lexing ListT.Attoparsec.ParsingFailure |
  -- | A token-stream parsing failure
  Error_Parsing ListT.HTMLParser.Error
  deriving (Show, Eq)
