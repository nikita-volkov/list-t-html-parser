module Main where

import BasePrelude
import Test.Hspec
import Conversion
import Conversion.Text
import Control.Monad.Trans.Either
import Data.Text (Text)
import qualified Data.XML.Types as XML
import qualified Data.Text.IO
import qualified HTMLTokenizer
import qualified ListT.Attoparsec
import qualified ListT.HTMLParser
import qualified ListT.Text
import qualified ListT.HTMLParser as P


main =
  hspec $ do
    it "text with spaces gets ignored" $ do
      result <- parse (P.openingTag *> P.closingTag) "<a>   </a>"
      shouldSatisfy result isRight
    it "text gets trimmed" $ do
      result <- parse (P.openingTag *> P.text <* P.closingTag) "<a>  b </a>"
      shouldBe result (Right "b")
    it "html consumes text" $ do
      result <- parse P.html "a<br/>"
      shouldBe result (Right "a")
    it "html consumes comment" $ do
      result <- parse P.html "<!--a-->b"
      shouldBe result (Right "<!--a-->")
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
    context "HTML with deep unclosed tags" $ do
      let text = "<a><b><br><br></b></a>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse (P.html) text
        it "should be proper" $ shouldBe result (Right "<a><b><br/><br/></b></a>")
    context "HTML with repetitive tags" $ do
      let text = "<a><a></a></a>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse (P.html) text
        it "should be proper" $ shouldBe result (Right "<a><a></a></a>")
    context "Broken closing tag" $ do
      let text = "<a></b></a>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse P.html text
        it "should be proper" $ shouldBe result (Right "<a></a>")
    context "Interspersed tag ending" $ do
      let text = "<a><b></a></b>"
      context "\"html\" parser result" $ do
        result <- runIO $ parse (mconcat <$> many P.html) text
        it "should be proper" $ shouldBe result (Right "<a><b/></a>")
    context "Complex HTML" $ do
      let text = "<p>a<strong>b</strong>c<br><br>d<br></p>"
      context "html" $ do
        result <- runIO $ parse (mconcat <$> many P.html) text
        it "should be correct" $ shouldBe result (Right "<p>a<strong>b</strong>c<br/><br/>d<br/></p>")
      context "xmlNode" $ do
        result <- runIO $ parse P.xmlNode text
        it "should be correct" $ shouldBe result $ Right $ 
          XML.NodeElement $ XML.Element "p" [] [
            XML.NodeContent (XML.ContentEntity "a"),
            XML.NodeElement $ XML.Element "strong" [] [
              XML.NodeContent (XML.ContentEntity "b")
              ],
            XML.NodeContent (XML.ContentEntity "c"),
            XML.NodeElement $ XML.Element "br" [] [],
            XML.NodeElement $ XML.Element "br" [] [],
            XML.NodeContent (XML.ContentEntity "d"),
            XML.NodeElement $ XML.Element "br" [] []
            ]

    context "Complex HTML 2" $ do
      let text = "<a><b>c</b><d><e>f<g><h>i<j>k</j><l></a>"
      result <- runIO $ parse (mconcat <$> many P.html) text
      it "should be correct" $ shouldBe result (Right "<a><b>c</b><d/><e/>f<g/><h/>i<j>k</j><l/></a>")
    context "Unclosed HTML" $ do
      let text = "<p>a<strong>b</strong>c<br><br>d<br>"
      result <- runIO $ parse (mconcat <$> many P.html) text
      it "should be correct" $ shouldBe result (Right "")
    context "Closing tag comming first" $ do
      let text = "</a><b></b>"
      result <- runIO $ parse P.properHTML text
      it "should fail" $ shouldSatisfy result isLeft
    context "HTML sample file #1" $ do
      text <- runIO $ Data.Text.IO.readFile "hspec/samples/1.html"
      it "should parse fine" $ do
        result <- parse (mconcat <$> many P.html) text
        shouldSatisfy result isRight
    context "Doc examples" $ do
      context "I'm not your guy" $ do
        let text = "<ul><li>I'm not your friend, <b>buddy</b>!</li><li>I'm not your buddy, <b>guy</b>!</li><li>He's not your guy, <b>friend</b>!</li><li>I'm not your friend, <b>buddy</b>!</li></ul>"
        it "Single" $ do
          Right result <- parse (P.openingTag *> P.html) text
          shouldBe result "<li>I'm not your friend, <b>buddy</b>!</li>"
        it "Multiple" $ do
          Right result <- parse (P.openingTag *> (mconcat <$> many P.properHTML) <* P.closingTag) text
          shouldBe result "<li>I'm not your friend, <b>buddy</b>!</li><li>I'm not your buddy, <b>guy</b>!</li><li>He's not your guy, <b>friend</b>!</li><li>I'm not your friend, <b>buddy</b>!</li>"

parse :: (forall m. Monad m => ListT.HTMLParser.Parser m a) -> Text -> IO (Either Error a)
parse parser =
  fmap (either (Left . Error_Lexing) id) . runEitherT .
  fmap (either (Left . Error_Parsing) Right) .
  ListT.HTMLParser.run parser . 
  ListT.Attoparsec.stream HTMLTokenizer.token . 
  ListT.Text.stream 2

data Error =
  -- | A tokenization failure
  Error_Lexing ListT.Attoparsec.Error |
  -- | A token-stream parsing failure
  Error_Parsing ListT.HTMLParser.Error
  deriving (Show, Eq)
