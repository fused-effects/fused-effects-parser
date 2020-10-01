{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Parser.Church
( -- * Parser carrier
  runParserWithString
, runParserWithFile
, runParserWith
, runParser
, ParserC(ParserC)
, emptyWith
, cutfailWith
, Input(..)
, pos_
, str_
, Err(..)
, input_
, reason_
, expected_
, errToNotice
  -- * Parser effect
, module Control.Effect.Parser
  -- * Cut effect
, module Control.Effect.Cut
) where

import           Control.Algebra
import           Control.Effect.Cut
import           Control.Effect.NonDet
import           Control.Effect.Parser
import           Control.Effect.Parser.Excerpt
import           Control.Effect.Parser.Lens
import qualified Control.Effect.Parser.Notice as Notice
import           Control.Effect.Parser.Source as Source
import           Control.Effect.Parser.Span as Span
import           Control.Effect.Throw
import           Control.Monad (ap)
import           Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Coerce (coerce)
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Maybe (fromMaybe)
import           Data.Set (Set, singleton, toList)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Text.Parser.Char (CharParsing(..))
import           Text.Parser.Combinators
import           Text.Parser.Token (TokenParsing)

runParserWithString :: Has (Throw Notice.Notice) sig m => Pos -> String -> ParserC m a -> m a
runParserWithString pos str = runParserWith Nothing (Input pos str)
{-# INLINE runParserWithString #-}

runParserWithFile :: (Has (Throw Notice.Notice) sig m, MonadIO m) => FilePath -> ParserC m a -> m a
runParserWithFile path p = do
  input <- liftIO (readFile path)
  runParserWith (Just path) (Input (Pos 0 0) input) p
{-# INLINE runParserWithFile #-}

runParserWith :: Has (Throw Notice.Notice) sig m => Maybe FilePath -> Input -> ParserC m a -> m a
runParserWith path input = runParser (const pure) failure failure input
  where
  src = sourceFromString path (str input)
  failure = throwError . errToNotice src
{-# INLINE runParserWith #-}

runParser
  :: (Input -> a -> m r)
  -> (Err -> m r)
  -> (Err -> m r)
  -> Input
  -> ParserC m a
  -> m r
runParser leaf nil fail input (ParserC run) = run leaf nil fail input
{-# INLINE runParser #-}


newtype ParserC m a = ParserC
  { runParserC
    :: forall r
    .  (Input -> a -> m r) -- success
    -> (Err -> m r)        -- empty
    -> (Err -> m r)        -- cut
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

  ParserC l <|> ParserC r = ParserC $ \ leaf nil fail i ->
    l
      leaf
      (\ el -> r
        leaf
        (nil . extend el)
        (fail . extend el)
        i)
      fail
      i
    where
    extend el er = er & reason_ %~ (<|> reason el) & if null (expected er) then expected_ .~ expected el else id
  {-# INLINE (<|>) #-}

instance Monad (ParserC m) where
  ParserC m >>= f = ParserC $ \ leaf nil fail i -> m (\ i' -> if pos i == pos i' then
    runParser leaf nil fail i' . f
  else
    runParser leaf fail fail i' . f) nil fail i
  {-# INLINE (>>=) #-}

instance Algebra sig m => Fail.MonadFail (ParserC m) where
  fail = unexpected
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ParserC m) where
  mfix f = ParserC $ \ leaf nil fail input ->
    mfix (distParser input . f . run . fromParser input . snd)
    >>= run . uncurry (runParser (fmap pure . leaf) (pure . nil) (pure . fail))
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

instance Parsing (ParserC m) where
  try m = ParserC $ \ leaf nil _ input -> runParser leaf nil nil input m
  {-# INLINE try #-}

  eof = notFollowedBy anyChar <?> "end of input"
  {-# INLINE eof #-}

  unexpected s = ParserC $ \ _ nil _ input -> nil (Err input (Just (pretty s)) mempty)
  {-# INLINE unexpected #-}

  m <?> s = ParserC $ \ leaf nil fail -> runParserC m
    leaf
    (nil  . (expected_ .~ singleton s))
    fail
  {-# INLINE (<?>) #-}

  notFollowedBy p = ParserC $ \ leaf nil _ input -> runParserC p
    (\ _ a -> nil (Err input (Just (pretty (show a))) mempty))
    (\ _ -> leaf input ())
    (\ _ -> leaf input ())
    input
  {-# INLINE notFollowedBy #-}

instance CharParsing (ParserC m) where
  satisfy p = acceptC (\ c -> if p c then Just c else Nothing)
  {-# INLINE satisfy #-}

instance TokenParsing (ParserC m)

acceptC :: (Char -> Maybe a) -> ParserC m a
acceptC p = ParserC $ \ leaf nil _ input -> case str input of
  c:_ | Just a <- p c -> leaf (advance input) a
      | otherwise     -> nil (Err input (Just (pretty "unexpected " <> pretty (show c))) mempty)
  _                   -> nil (Err input (Just (pretty "unexpected end of input")) mempty)
{-# INLINE acceptC #-}

instance Algebra sig m => Algebra (Parser :+: Cut :+: NonDet :+: sig) (ParserC m) where
  alg hdl sig ctx = case sig of
    L (Accept p)         -> (<$ ctx) <$> acceptC p

    L (Label m s)        -> hdl (m <$ ctx) <?> s

    L (Unexpected s)     -> unexpected s

    L Position           -> ParserC $ \ leaf _ _ input -> leaf input (pos input <$ ctx)

    R (L Cutfail)        -> ParserC $ \ _ _ fail input -> fail (Err input Nothing mempty)

    R (L (Call m))       -> try (hdl (m <$ ctx))

    R (R (L (L Empty)))  -> empty

    R (R (L (R Choose))) -> pure (True <$ ctx) <|> pure (False <$ ctx)

    R (R (R other))      -> ParserC $ \ leaf nil fail input ->
      thread (fmap Compose . uncurry dst . getCompose ~<~ hdl) other (Compose (input, pure ctx))
      >>= runIdentity . uncurry (runParser (coerce leaf) (coerce nil) (coerce fail)) . getCompose
    where
    dst :: Applicative m => Input -> ParserC Identity (ParserC m a) -> m (Input, ParserC Identity a)
    dst = fmap runIdentity
        . runParser
          (\ i -> pure . distParser i)
          (pure . emptyk)
          (pure . cutfailk)
  {-# INLINE alg #-}

distParser :: Applicative m => Input -> ParserC m a -> m (Input, ParserC Identity a)
distParser = runParser purek emptyk cutfailk
{-# INLINE distParser #-}

purek :: Applicative m => Input -> a -> m (Input, ParserC n a)
purek    i a = pure (i, pure a)
{-# INLINE purek #-}

emptyk :: Applicative m => Err -> m (Input, ParserC n a)
emptyk   Err{ input, reason, expected } = pure (input, emptyWith   reason expected)
{-# INLINE emptyk #-}

cutfailk :: Applicative m => Err -> m (Input, ParserC n a)
cutfailk Err{ input, reason, expected } = pure (input, cutfailWith reason expected)
{-# INLINE cutfailk #-}


-- | Fail to parse, providing the given document as a reason.
--
-- @
-- 'emptyWith' 'Nothing' 'mempty' = 'empty'
-- @
emptyWith :: Maybe (Doc AnsiStyle) -> Set String -> ParserC m a
emptyWith   a e = ParserC (\ _ nil _    i -> nil  (Err i a e))
{-# INLINE emptyWith #-}

-- | Fail to parse and prevent backtracking, providing the given document as a reason.
--
-- @
-- 'cutfailWith' 'Nothing' 'mempty' = 'cutfail'
-- @
cutfailWith :: Maybe (Doc AnsiStyle) -> Set String -> ParserC m a
cutfailWith a e = ParserC (\ _ _   fail i -> fail (Err i a e))
{-# INLINE cutfailWith #-}


data Input = Input
  { pos :: {-# UNPACK #-} !Pos
  , str :: !String
  }
  deriving (Eq, Ord, Show)

pos_ :: Lens' Input Pos
pos_ = lens pos $ \ i pos -> i{ pos }
{-# INLINE pos_ #-}

str_ :: Lens' Input String
str_ = lens str $ \ i str -> i{ str }
{-# INLINE str_ #-}

advance :: Input -> Input
advance = \case
  Input pos (c:cs) -> Input (advancePos c pos) cs
  i                -> i
{-# INLINE advance #-}

advancePos :: Char -> Pos -> Pos
advancePos c p = case c of
  -- FIXME: this should handle CR & maybe CRLF
  '\n' -> Pos (succ (Span.line p)) 0
  _    -> p { Span.column = succ (Span.column p) }
{-# INLINE advancePos #-}


data Err = Err
  { input    :: {-# UNPACK #-} !Input
  , reason   :: !(Maybe (Doc AnsiStyle))
  , expected :: !(Set String)
  }
  deriving (Show)

input_ :: Lens' Err Input
input_ = lens input $ \ i input -> i{ input }
{-# INLINE input_ #-}

reason_ :: Lens' Err (Maybe (Doc AnsiStyle))
reason_ = lens reason $ \ i reason -> i{ reason }
{-# INLINE reason_ #-}

expected_ :: Lens' Err (Set String)
expected_ = lens expected $ \ i expected -> i{ expected }
{-# INLINE expected_ #-}

errToNotice :: Source -> Err -> Notice.Notice
errToNotice source Err{ input = Input pos _, reason, expected } = Notice.Notice
  { level   = Just Notice.Error
  , excerpt = Excerpt (Source.path source) (source ! pos) (Span pos pos)
  , reason  = fromMaybe (fillSep (map pretty (words "unknown error"))) reason <> if null expected then mempty else comma <+> fillSep (pretty "expected" <> colon : punctuate comma (map pretty (toList expected)))
  , context = []
  }
{-# INLINE errToNotice #-}
