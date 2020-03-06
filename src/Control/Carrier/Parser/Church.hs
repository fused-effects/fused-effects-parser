{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  runParserWithString
, runParserWithFile
, runParserWith
, runParser
, Input(..)
, ParserC(..)
, emptyWith
, cutfailWith
  -- * Parser effect
, module Control.Effect.Parser
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Cut
import Control.Effect.NonDet
import Control.Effect.Parser
import Control.Effect.Parser.Excerpt
import Control.Effect.Parser.Notice
import Control.Effect.Throw
import Control.Monad (ap)
import Control.Monad.Fix
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Source.Span as Span
import Text.Parser.Char (CharParsing(..))
import Text.Parser.Combinators
import Text.Parser.Token (TokenParsing)

runParserWithString :: Has (Throw Notice) sig m => Pos -> String -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m a
runParserWithString pos input p = runParserWith "(interactive)" (Input pos input) p >>= either throwError pure
{-# INLINE runParserWithString #-}

runParserWithFile :: (Has (Throw Notice) sig m, MonadIO m) => FilePath -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m a
runParserWithFile path p = do
  input <- liftIO (readFile path)
  runParserWith path (Input (Pos 0 0) input) p >>= either throwError pure
{-# INLINE runParserWithFile #-}

runParserWith :: Applicative m => FilePath -> Input -> ParserC (ReaderC Path (ReaderC Lines m)) a -> m (Either Notice a)
runParserWith path input = runReader (Lines inputLines) . runReader (Path path) . runParser success failure failure input
  where
  success _ a = pure (Right a)
  failure (Input pos _) reason expected = pure (Left (Notice (Just Error) (Excerpt path (inputLines !! Span.line pos) (Span pos pos)) (fromMaybe (pretty "unknown error") reason) expected []))
  inputLines = lines (str input)
  lines "" = [""]
  lines s  = let (line, rest) = takeLine s in line : lines rest
  takeLine ""          = ("", "")
  takeLine ('\n':rest) = ("\n", rest)
  takeLine (c   :rest) = let (cs, rest') = takeLine rest in (c:cs, rest')
{-# INLINE runParserWith #-}

runParser
  :: (Input -> a -> m r)
  -> (Input -> Maybe (Doc AnsiStyle) -> Set String -> m r)
  -> (Input -> Maybe (Doc AnsiStyle) -> Set String -> m r)
  -> Input
  -> ParserC m a
  -> m r
runParser leaf nil fail input (ParserC run) = run leaf nil fail input
{-# INLINE runParser #-}

data Input = Input
  { pos :: {-# UNPACK #-} !Pos
  , str :: !String
  }
  deriving (Eq, Ord, Show)

newtype ParserC m a = ParserC
  { runParserC
    :: forall r
    .  (Input -> a -> m r)                                   -- success
    -> (Input -> Maybe (Doc AnsiStyle) -> Set String -> m r) -- empty
    -> (Input -> Maybe (Doc AnsiStyle) -> Set String -> m r) -- cut
    -> Input
    -> m r
  }
  deriving (Functor)

instance Applicative (ParserC m) where
  pure a = ParserC (\ leaf _ _ input -> leaf input a)
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Alternative (ParserC m) where
  empty = emptyWith Nothing mempty
  {-# INLINE empty #-}

  ParserC l <|> ParserC r = ParserC (\ leaf nil fail input -> l leaf (const (\ e a -> r leaf (\ i e' a' -> nil i (e' <|> e) (a <> a')) fail input)) fail input)
  {-# INLINE (<|>) #-}

instance Monad (ParserC m) where
  ParserC m >>= f = ParserC (\ leaf nil fail -> m (\ input -> runParser leaf nil fail input . f) nil fail)
  {-# INLINE (>>=) #-}

instance (Algebra sig m, Effect sig) => Fail.MonadFail (ParserC m) where
  fail = unexpected
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ParserC m) where
  mfix f = ParserC $ \ leaf nil fail input ->
    mfix (distParser input . f . run . fromParser input . snd)
    >>= run . uncurry (runParser (fmap pure . leaf) (\ i -> fmap pure . nil i) (\ i -> fmap pure . fail i))
    where
    fromParser = runParser (const pure) (error "mfix ParserC: empty") (error "mfix ParserC: cutfail")
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ParserC m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadPlus (ParserC m)

instance MonadTrans ParserC where
  lift m = ParserC $ \ leaf _ _ input -> m >>= leaf input
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Parsing (ParserC m) where
  try = call
  {-# INLINE try #-}

  eof = notFollowedBy anyChar <?> "end of input"
  {-# INLINE eof #-}

  unexpected s = send (Unexpected s)
  {-# INLINE unexpected #-}

  m <?> s = send (Label m s pure)
  {-# INLINE (<?>) #-}

  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))
  {-# INLINE notFollowedBy #-}

instance (Algebra sig m, Effect sig) => CharParsing (ParserC m) where
  satisfy p = accept (\ c -> if p c then Just c else Nothing)
  {-# INLINE satisfy #-}

instance (Algebra sig m, Effect sig) => TokenParsing (ParserC m)

instance (Algebra sig m, Effect sig) => Algebra (Parser :+: Cut :+: NonDet :+: sig) (ParserC m) where
  alg = \case
    L parser -> case parser of
      Accept p k   ->
        ParserC (\ leaf nil _ input -> case str input of
          c:_ | Just a <- p c -> leaf (advance input) a
              | otherwise     -> nil input (Just (pretty "unexpected " <> pretty c)) mempty
          _                   -> nil input (Just (pretty "unexpected EOF")) mempty)
        >>= k

      Label m s k  ->
        ParserC (\ leaf nil fail -> runParserC m leaf
          (\ i -> nil  i . (<|> Just (pretty s)))
          (\ i -> fail i . (<|> Just (pretty s))))
        >>= k

      Unexpected s -> ParserC $ \ _ nil _ input -> nil input (Just (pretty s)) mempty

      Position k   ->
        ParserC (\ leaf _ _ input -> leaf input (pos input))
        >>= k

    R (L cut) -> case cut of
      Cutfail  -> cutfailWith Nothing mempty

      Call m k ->
        ParserC (\ leaf nil _ -> runParserC m leaf nil nil)
        >>= k

    R (R (L nondet)) -> case nondet of
      L Empty      -> empty

      R (Choose k) -> k True <|> k False

    R (R (R other)) -> ParserC $ \ leaf nil fail input ->
      alg (thread (Compose (input, pure ())) (fmap Compose . uncurry dst . getCompose) other)
      >>= runIdentity . uncurry (runParser (coerce leaf) (coerce nil) (coerce fail)) . getCompose
    where
    dst :: Input -> ParserC Identity (ParserC m a) -> m (Input, ParserC Identity a)
    dst = fmap runIdentity
        . runParser
          (fmap pure . distParser)
          (\ i -> fmap pure . emptyk i)
          (\ i -> fmap pure . cutfailk i)
  {-# INLINE alg #-}

distParser :: Applicative m => Input -> ParserC m a -> m (Input, ParserC Identity a)
distParser = runParser purek emptyk cutfailk
{-# INLINE distParser #-}

purek :: Applicative m => Input -> a -> m (Input, ParserC n a)
purek    i a = pure (i, pure a)
{-# INLINE purek #-}

emptyk :: Applicative m => Input -> Maybe (Doc AnsiStyle) -> Set String -> m (Input, ParserC n a)
emptyk   i a e = pure (i, emptyWith a e)
{-# INLINE emptyk #-}

cutfailk :: Applicative m => Input -> Maybe (Doc AnsiStyle) -> Set String -> m (Input, ParserC n a)
cutfailk i a e = pure (i, cutfailWith a e)
{-# INLINE cutfailk #-}


-- | Fail to parse, providing the given document as a reason.
--
-- @
-- 'emptyWith' 'Nothing' 'mempty' = 'empty'
-- @
emptyWith :: Maybe (Doc AnsiStyle) -> Set String -> ParserC m a
emptyWith a e = ParserC (\ _ nil _    i -> nil  i a e)
{-# INLINE emptyWith #-}

-- | Fail to parse and prevent backtracking, providing the given document as a reason.
--
-- @
-- 'cutfailWith' 'Nothing' 'mempty' = 'cutfail'
-- @
cutfailWith :: Maybe (Doc AnsiStyle) -> Set String -> ParserC m a
cutfailWith a e = ParserC (\ _ _   fail i -> fail i a e)
{-# INLINE cutfailWith #-}


advance :: Input -> Input
advance (Input pos (c:cs)) = Input (advancePos c pos) cs
advance i                  = i
{-# INLINE advance #-}

advancePos :: Char -> Pos -> Pos
advancePos '\n' p = Pos (succ (Span.line p)) 0
advancePos _    p = p { Span.column = succ (Span.column p) }
{-# INLINE advancePos #-}
