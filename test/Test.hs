{-# LANGUAGE NamedFieldPuns #-}
module Main
( main
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Parser.Church
import Control.Carrier.Reader
import Control.Effect.Parser.Notice as Notice
import Data.Set
import Source.Span (Pos(..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parser.Char
import Text.Parser.Combinators

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ parserTests
  ]

parserTests :: TestTree
parserTests = testGroup "ParserC (Church)"
  [ testGroup "position"
    [ testCase "at start" $
      parsesInto position "" (Pos 0 0)

    , testCase "at end" $
      parsesInto (char 'x' *> position) "x" (Pos 0 1)

    , testCase "after newline" $
      parsesInto (newline *> position) "\n" (Pos 1 0)
    ]
  , testGroup "<?>"
    [ testCase "replaces labels" $
      failsWith (char 'a' <?> "c") "b" (\ Notice{ Notice.expected } -> expected @?= singleton "c")
    , testCase "applies outermost" $
      failsWith ((char 'a' <?> "b") <?> "c") "d" (\ Notice{ Notice.expected } -> expected @?= singleton "c")
    , testCase "is joined by <|>" $
      failsWith ((char 'a' <?> "b") <|> (char 'c' <?> "d")) "e" (\ Notice{ Notice.expected } -> expected @?= fromList ["b", "d"])
    , testCase "replaces joined labels <|>" $
      failsWith (((char 'a' <?> "b") <|> (char 'c' <?> "d")) <?> "e") "f" (\ Notice{ Notice.expected } -> expected @?= singleton "e")
    ]
  ]


parsesInto :: (Eq a, Show a) => ParserC (ReaderC Path (ReaderC Lines (Either Notice))) a -> String -> a -> Assertion
parsesInto p s expected = case runParserWithString (Pos 0 0) s p of
  Left  err    -> assertFailure (show err)
  Right actual -> actual @?= expected

failsWith :: Show a => ParserC (ReaderC Path (ReaderC Lines (Either Notice))) a -> String -> (Notice -> Assertion) -> Assertion
failsWith p s f = case runParserWithString (Pos 0 0) s p of
  Left  err    -> f err
  Right actual -> assertFailure (show actual)
