module Main where

import BasePrelude
import Test.Hspec
import Conversion
import Conversion.Text
import Data.Text (Text)
import qualified Data.Text.IO
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
    context "HTML with unclosed tags" $ do
      let text = "<a><br><br></a>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse (P.html) text
        it "should not be failure" $ shouldSatisfy result isRight
        it "should be proper" $ shouldBe result (Right "<a><br/><br/></a>")
    context "HTML with deep tags" $ do
      let text = "<a><a><br><br></a></a>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse (P.html) text
        it "should be proper" $ shouldBe result (Right "<a><a><br/><br/></a></a>")
    context "Broken closing tag" $ do
      let text = "<a></b></a>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse P.html text
        it "should be proper" $ shouldBe result (Right "<a></a>")
    context "Interspersed tag ending" $ do
      let text = "<a><b></a></b>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse (mconcat <$> many P.html) text
        it "should be proper" $ shouldBe result (Right "<a/><b></b>")
    -- context "HTML sample file #1" $ do
    --   text <- runIO $ Data.Text.IO.readFile "hspec/samples/1.html"
    --   context "Running the \"html\" parser on it" $ do
    --     result <- runIO $ parse (P.token *> P.html) text
    --     it "should not fail" $ shouldSatisfy result isRight

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
