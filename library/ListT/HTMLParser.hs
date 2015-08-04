module ListT.HTMLParser
(
  Parser,
  Error,
  ErrorDetails(..),
  run,
  -- * Parsers
  eoi,
  token,
  rawToken,
  space,
  openingTag,
  closingTag,
  text,
  comment,
  html,
  properHTML,
  xmlNode,
  -- * Combinators
  many1,
  manyTill,
  skipTill,
  total,
)
where

import BasePrelude
import MTLPrelude hiding (Error, shift)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.Text (Text)
import Conversion
import Conversion.Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified ListT as L
import qualified HTMLTokenizer as HT
import qualified HTMLEntities.Decoder
import qualified ListT.HTMLParser.Renderer as Renderer
import qualified ListT.HTMLParser.XML as XML
import qualified Data.XML.Types as XMLTypes

-- |
-- A backtracking HTML-tokens stream parser.
newtype Parser m a =
  Parser { unwrap :: EitherT Error (StateT (ListT m HT.Token, [HT.Token]) m) a }
  deriving (Functor, Applicative, MonadError Error)

-- |
-- A possibly detailed parser error.
-- When 'mzero' or 'empty' is used, an error value of 'Nothing' is produced.
type Error =
  Maybe ErrorDetails

data ErrorDetails =
  -- | A text message
  ErrorDetails_Message Text |
  -- | Unexpected token
  ErrorDetails_UnexpectedToken |
  -- | End of input
  ErrorDetails_EOI
  deriving (Show, Eq)

instance Monad m => Monad (Parser m) where
  return =
    Parser . return
  (>>=) a b =
    Parser $ unwrap a >>= unwrap . b
  fail a =
    throwError $ Just $ ErrorDetails_Message $ fromString a

instance Monad m => Alternative (Parser m) where
  empty =
    Parser $ EitherT $ return $ Left Nothing
  (<|>) a b =
    Parser $ EitherT $ StateT $ \(incoming, backtrack) -> do
      (result', (incoming', backtrack')) <- flip runStateT (incoming, []) $ runEitherT $ unwrap $ a
      (result'', (incoming'', backtrack'')) <-
        case result' of
          Left _ -> do
            flip runStateT (foldl' (flip L.cons) incoming' backtrack', []) $ runEitherT $ unwrap $ b
          Right result' -> do
            return (Right result', (incoming', backtrack'))
      return (result'', (incoming'', backtrack'' <> backtrack))

instance Monad m => MonadPlus (Parser m) where
  mzero = empty
  mplus = (<|>)

-- |
-- Run a parser on a stream of HTML tokens,
-- consuming only as many as needed.
run :: Monad m => Parser m a -> ListT m HT.Token -> m (Either Error a)
run p l =
  flip evalStateT (l, []) $ runEitherT $ unwrap $ p

-- |
-- An HTML token as it is: without HTML-decoding and ignoring of spaces.
rawToken :: Monad m => Parser m HT.Token
rawToken =
  Parser $ EitherT $ StateT $ \(incoming, backtrack) -> 
  liftM (maybe (Left (Just ErrorDetails_EOI), (incoming, backtrack)) 
               (\(a, incoming') -> (Right a, (incoming', a : backtrack)))) $ 
  L.uncons incoming

-- |
-- A token with HTML entities decoded and with spaces filtered out.
token :: Monad m => Parser m HT.Token
token =
  rawToken >>= \case
    HT.Token_Text x -> Text.strip x & \x -> if Text.null x 
      then token 
      else return $ HT.Token_Text $ convert $ decode x
    HT.Token_Comment x -> return $ HT.Token_Comment $ convert $ decode x
    HT.Token_OpeningTag (name, attrs, closed) -> 
      return $ HT.Token_OpeningTag $ (name, ((fmap . fmap . fmap) (convert . decode) attrs), closed)
    x -> return x
  where
    decode = HTMLEntities.Decoder.htmlEncodedText


-- |
-- A text token, which is completely composed of characters,
-- which satisfy the 'isSpace' predicate.
space :: Monad m => Parser m Text
space =
  rawToken >>= \case
    HT.Token_Text x | Text.all isSpace x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- End of input.
eoi :: Monad m => Parser m ()
eoi =
  rawToken $> () <|> pure ()

-- |
-- An opening tag with HTML entities in values decoded.
openingTag :: Monad m => Parser m HT.OpeningTag
openingTag =
  token >>= \case
    HT.Token_OpeningTag x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- A closing tag.
closingTag :: Monad m => Parser m HT.Identifier
closingTag =
  token >>= \case
    HT.Token_ClosingTag x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- A text between tags with HTML entities decoded.
text :: Monad m => Parser m Text
text =
  token >>= \case
    HT.Token_Text x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- Contents of a comment.
comment :: Monad m => Parser m Text
comment =
  token >>= \case
    HT.Token_Comment x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- Apply a parser at least one time.
many1 :: Monad m => Parser m a -> Parser m [a]
many1 a =
  (:) <$> a <*> many a

-- |
-- Apply a parser multiple times until another parser is satisfied.
-- Returns results of both parsers.
manyTill :: Monad m => Parser m a -> Parser m b -> Parser m ([a], b)
manyTill a b =
  fix $ \loop -> 
    ([],) <$> b <|> 
    (\a (al, b) -> (a : al, b)) <$> a <*> loop

-- |
-- Skip any tokens until the provided parser is satisfied.
skipTill :: Monad m => Parser m a -> Parser m a
skipTill a =
  fix $ \loop ->
    a <|> (rawToken *> loop)

-- |
-- Greedily consume all the input until the end,
-- while running the provided parser.
-- Same as:
-- 
-- > theParser <* eoi
total :: Monad m => Parser m a -> Parser m a
total a =
  a <* eoi

-- |
-- The auto-repaired textual HTML representation of an HTML-tree node.
-- 
-- Useful for consuming HTML-formatted snippets.
-- 
-- E.g., when the following parser:
-- 
-- > openingTag *> html
-- 
-- is run against the following HTML snippet:
-- 
-- > <ul>
-- >   <li>I'm not your friend, <b>buddy</b>!</li>
-- >   <li>I'm not your buddy, <b>guy</b>!</li>
-- >   <li>He's not your guy, <b>friend</b>!</li>
-- >   <li>I'm not your friend, <b>buddy</b>!</li>
-- > </ul>
-- 
-- it'll produce the following text builder value:
-- 
-- > <li>I'm not your friend, <b>buddy</b>!</li>
-- 
-- If you want to consume all children of a node,
-- it's recommended to use 'properHTML' in combination with 'many' or 'many1'.
-- For details consult the docs on 'properHTML'.
-- 
-- __This parser is smart and handles and repairs broken HTML__:
-- 
-- * It repairs unclosed tags,
-- interpreting them as closed singletons. 
-- E.g., @\<br\>@ will be consumed as @\<br\/\>@.
-- 
-- * It handles orphan closing tags by ignoring them.
-- E.g. it'll consume the input @\<a\>\<\/b\>\<\/a\>@ as @\<a\>\<\/a\>@.
html :: Monad m => Parser m Text.Builder
html =
  flip fmap cleanTokenSequence $ 
  foldl' (flip mappend) mempty . map Renderer.token

-- |
-- Same as 'html', but fails if the input begins with an orphan closing tag.
-- I.e., the input \"\<\/a\>\<b\>\<\/b\>\" will make this parser fail.
-- 
-- This parser is particularly useful for consuming all children in the current context.
-- E.g., running the following parser:
-- 
-- > openingTag *> (mconcat <$> many properHTML)
-- 
-- on the following input:
-- 
-- > <ul>
-- >   <li>I'm not your friend, <b>buddy</b>!</li>
-- >   <li>I'm not your buddy, <b>guy</b>!</li>
-- >   <li>He's not your guy, <b>friend</b>!</li>
-- >   <li>I'm not your friend, <b>buddy</b>!</li>
-- > </ul>
-- 
-- will produce a merged text builder, which consists of the following nodes:
-- 
-- >   <li>I'm not your friend, <b>buddy</b>!</li>
-- >   <li>I'm not your buddy, <b>guy</b>!</li>
-- >   <li>He's not your guy, <b>friend</b>!</li>
-- >   <li>I'm not your friend, <b>buddy</b>!</li>
-- 
-- Notice that unlike with 'html', it's safe to assume 
-- that it will not consume the following closing @\<\/ul\>@ tag,
-- because it does not begin a valid HTML-tree node.
-- 
-- Notice also that despite failing in case of the first broken token,
-- this parser handles the broken tokens in other cases the same way as 'html'.
properHTML :: Monad m => Parser m Text.Builder
properHTML =
  cleanTokenSequence >>= \case
    [] -> throwError $ Just $ ErrorDetails_Message "Improper HTML node"
    l -> return $ foldl' (flip mappend) mempty $ map Renderer.token l

-- |
-- Works the same way as 'properHTML', but constructs an XML-tree.
xmlNode :: Monad m => Parser m XMLTypes.Node
xmlNode =
  cleanTokenSequence >>= \case
    [] -> throwError $ Just $ ErrorDetails_Message "Improper HTML node"
    tokens -> XML.run XML.node (reverse tokens) & \case
      Just node -> return node
      -- If it's `Nothing`, then it's a bug.

cleanTokenSequence :: Monad m => Parser m [HT.Token]
cleanTokenSequence =
  fmap (fmap (either id id)) $
  flip execStateT [] $ fix $ \loop -> lift rawToken >>= \case
    HT.Token_Doctype _ -> return ()
    HT.Token_ClosingTag ct -> do
      ours <- state $ \list -> fromMaybe ([], list) $ do
        (l, r) <- Just $ flip break list $ \case
          Right (HT.Token_OpeningTag (n, _, False)) -> n == ct
          _ -> False
        (h, t) <- uncons r
        return (fmap (fmap closeOpeningTag) l <> [h], t)
      loop' <- bool loop (return ()) . null <$> get
      if null ours
        then loop'
        else do
          modify $ mappend $ (:) (Left (HT.Token_ClosingTag ct)) $ fmap (either Left Left) $ ours
          loop'
    t@(HT.Token_OpeningTag _) -> do
      modify $ (:) $ Right t
      loop
    t -> do
      context <- get
      modify $ (:) $ Right t
      if null context
        then return ()
        else loop
  where
    closeOpeningTag =
      \case
        HT.Token_OpeningTag (n, a, _) -> HT.Token_OpeningTag (n, a, True)
        x -> x