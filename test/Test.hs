{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
( main
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Parser.Church
import           Control.Effect.Parser.Source
import           Control.Effect.Parser.Span (Pos(..))
import           Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import           Data.Set
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Prelude hiding (lines)
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
import           Text.Parser.Char
import           Text.Parser.Combinators

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ parserTests
  , testGroup "Source"
    [ testGroup "sourceFromString"
      [ testCase "returns the empty string for the empty string" $
        sourceFromString Nothing 0 "" @?= Source Nothing (NE.fromList [Line 0 "" EOF])
      , testCase "returns two empty strings for a newline" $
        sourceFromString Nothing 0 "\n" @?= Source Nothing (NE.fromList [Line 0 "" LF, Line 1 "" EOF])
      , testProperty "returns one more string than there are newlines" . property $ do
        s <- forAll (Gen.string (Range.linear 1 100)
          (Gen.frequency [ (5, Gen.unicode), (1, Gen.element "\t\r\n ") ]))
        length (lines (sourceFromString Nothing 0 s))
          === length (Prelude.filter (`elem` "\r\n") (replace "\r\n" "\n" s)) + 1
      ]
    ]
  ]
  where
  replace a b = go
    where
    go = \case
      ""                        -> ""
      c:cs
        | a `isPrefixOf` (c:cs) -> b <> go (Prelude.drop (length a) (c:cs))
        | otherwise             -> c : go cs

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
      failsWith (char 'a' <?> "c") "b" (hasExpectation (singleton "c"))
    , testCase "applies outermost" $
      failsWith ((char 'a' <?> "b") <?> "c") "d" (hasExpectation (singleton "c"))
    , testCase "is joined by <|>" $
      failsWith ((char 'a' <?> "b") <|> (char 'c' <?> "d")) "e" (hasExpectation (fromList ["b", "d"]))
    , testCase "replaces joined labels <|>" $
      failsWith (((char 'a' <?> "b") <|> (char 'c' <?> "d")) <?> "e") "f" (hasExpectation (singleton "e"))
    ]
  ]


parsesInto :: (Eq a, Show a) => ParserC (Either (Source, Err)) a -> String -> a -> Assertion
parsesInto p s expected = case runParserWithString (Pos 0 0) s p of
  Left  (s, e) -> assertFailure (show (errToNotice s e))
  Right actual -> actual @?= expected

failsWith :: Show a => ParserC IO a -> String -> (Err -> Assertion) -> Assertion
failsWith p s f = runParser (const (assertFailure . show)) f f (Input (Pos 0 0) s) p

hasExpectation :: Set String -> Err -> Assertion
hasExpectation expected' Err{ expected } = expected @?= expected'
