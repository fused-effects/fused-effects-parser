module Control.Effect.Parser.GHCI
( parse
, sig
) where

import Control.Applicative
import Control.Carrier.Parser.Church
import Control.Effect.Parser.Notice
import Control.Effect.Parser.Span hiding (line)
import Debug.Trace
import Prettyprinter (line)
import Prettyprinter.Render.Terminal
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

parse :: Show a => ParserC (Either Notice) a -> String -> IO ()
parse p s = do
  let v = runParserWithString (Pos 0 0) s p
  either (putDoc . (<> line) . prettyNotice) print v

sig :: (TokenParsing m, Monad m) => m String
sig = sig' _Type _Type

sig' :: (TokenParsing m, Monad m) => m String -> m String -> m String
sig' t e
  =   q t e
  <|> f t e
  <|> (++) <$> type' t <*> expr

q t e = do { v <-      braces (tvar <* colon <* type' t)  <* arrow ; traceM v ; sig' (var v <|> t) e } <?> "forall"
f t e = do { v <- try (symbolic '(' *> evar) <* colon <* type' t <* symbolic ')' <* arrow ; traceM v ; sig' t (var v <|> e) } <?> "function type"

type' :: TokenParsing m => m String -> m String
type' v = v <|> _Type <|> parens (type' v) <?> "type"

arrow :: TokenParsing m => m String
arrow = symbol "->"

expr :: TokenParsing m => m String
expr = braces (pure "") <?> "expr"

evar :: TokenParsing m => m String
evar = token ((:) <$> lower <*> many alphaNum) <?> "evar"

tvar :: TokenParsing m => m String
tvar = token ((:) <$> upper <*> many alphaNum) <?> "tvar"

_Type :: TokenParsing m => m String
_Type = token (symbol "Type") <?> "Type"

var :: TokenParsing m => String -> m String
var v = token (symbol v) <?> v
