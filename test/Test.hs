module Main
( main
) where

import Control.Carrier.Parser.Church
import Control.Carrier.Reader
import Control.Effect.Parser.Notice
import Source.Span (Pos(..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parser.Char

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
  ]


parsesInto :: (Eq a, Show a) => ParserC (ReaderC Path (ReaderC Lines (Either Notice))) a -> String -> a -> Assertion
parsesInto p s expected = case runParserWithString (Pos 0 0) s p of
  Left  err    -> assertFailure (show err)
  Right actual -> actual @?= expected
