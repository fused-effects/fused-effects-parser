module Main
( main
) where

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ parserTests
  ]

parserTests :: TestTree
parserTests = testGroup "ParserC (Church)"
  []
