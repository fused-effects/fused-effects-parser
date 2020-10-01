module Control.Effect.Parser.GHCI
( parse
) where

import Control.Effect.Parser.Notice
import Control.Effect.Parser.Span
import Control.Carrier.Parser.Church

parse :: ParserC (Either Notice) a -> String -> Either Notice a
parse p s = runParserWithString (Pos 0 0) s p
