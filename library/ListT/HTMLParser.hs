module ListT.HTMLParser
(
  Parser,
  Error,
  ErrorDetails(..),
  run,
  -- * Parsers
  eoi,
  token,
  openingTag,
  closingTag,
  text,
  comment,
  html,
  -- * Combinators
  manyTill,
  skipTill,
  total,
)
where

import BasePrelude hiding (uncons, cons)
import MTLPrelude hiding (Error, shift)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified ListT as L
import qualified HTMLTokenizer.Parser as HT


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
  deriving (Show)

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
      (aResult, (incoming', backtrack')) <- flip runStateT (incoming, []) $ runEitherT $ unwrap $ a
      case aResult of
        Left _ -> do
          flip runStateT (foldr L.cons incoming' backtrack', []) $ runEitherT $ unwrap $ b
        Right aResult -> do
          return (Right aResult, (incoming', backtrack'))

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
-- End of input.
eoi :: Monad m => Parser m ()
eoi =
  token $> () <|> pure ()

-- |
-- Any HTML token.
token :: Monad m => Parser m HT.Token
token =
  Parser $ EitherT $ StateT $ \(incoming, backtrack) -> 
  liftM (maybe (Left (Just ErrorDetails_EOI), (incoming, backtrack)) 
               (\(a, incoming') -> (Right a, (incoming', a : backtrack)))) $ 
  L.uncons incoming

-- |
-- An opening tag.
openingTag :: Monad m => Parser m HT.OpeningTag
openingTag =
  token >>= \case
    HT.Token_OpeningTag x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- A closing tag.
closingTag :: Monad m => Parser m HT.ClosingTag
closingTag =
  token >>= \case
    HT.Token_ClosingTag x -> return x
    _ -> throwError (Just ErrorDetails_UnexpectedToken)

-- |
-- A text between tags with HTML-entities decoded.
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
    a <|> (token *> loop)

-- |
-- Greedily consume all the input until the end,
-- while running the provided parser.
-- Same as:
-- 
-- > theParser <* eoi
total :: Monad m => Parser m a -> Parser m a
total a =
  a <* eoi

html :: Monad m => Parser m Text.Builder
html =
  undefined
