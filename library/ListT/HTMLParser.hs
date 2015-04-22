module ListT.HTMLParser
(
  Parser,
  Error,
  ErrorDetails(..),
  run,
  -- * Parsers
  token,
  openingTag,
  closingTag,
  text,
  comment,
  -- * Combinators
  manyTill,
  skipTill,
)
where

import BasePrelude hiding (uncons, cons)
import MTLPrelude hiding (Error, shift)
import Control.Monad.Trans.Either hiding (left, right)
import ListT (ListT)
import Data.Text (Text)
import qualified ListT as L
import qualified HTMLTokenizer.Parser as HT


-- |
-- A backtracking HTML parser.
newtype Parser m a =
  Parser { unwrap :: EitherT Error (StateT (ListT m HT.Token, [HT.Token]) m) a }
  deriving (Functor, Applicative, Monad, MonadError Error)

-- | 
type Error =
  Maybe ErrorDetails

data ErrorDetails =
  -- | Unexpected token
  UnexpectedToken |
  -- | End of input
  EOI
  deriving (Show)


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

token :: Monad m => Parser m HT.Token
token =
  Parser $ EitherT $ StateT $ \(incoming, backtrack) -> 
  liftM (maybe (Left (Just EOI), (incoming, backtrack)) 
               (\(a, incoming') -> (Right a, (incoming', a : backtrack)))) $ 
  L.uncons incoming

openingTag :: Monad m => Parser m HT.OpeningTag
openingTag =
  token >>= \case
    HT.Token_OpeningTag x -> return x
    _ -> throwError (Just UnexpectedToken)

closingTag :: Monad m => Parser m HT.ClosingTag
closingTag =
  token >>= \case
    HT.Token_ClosingTag x -> return x
    _ -> throwError (Just UnexpectedToken)

text :: Monad m => Parser m Text
text =
  token >>= \case
    HT.Token_Text x -> return x
    _ -> throwError (Just UnexpectedToken)

comment :: Monad m => Parser m Text
comment =
  token >>= \case
    HT.Token_Comment x -> return x
    _ -> throwError (Just UnexpectedToken)

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

