{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec (parse)

import Parser

parseExpr' :: Text -> Either String (Expr ())
parseExpr' i = toExpr <$> parseExprPretty "" i

test :: Text -> Expr () -> Expectation
test i e = do
  case parseExpr' i of
    (Right r) -> r `shouldBe` e
    (Left err) -> expectationFailure err

main :: IO ()
main = hspec $ do
  describe "Expression parser" $ do
    it "parses identifiers" $ do
      parse identifier "" "x" `shouldBe` Right "x"
    it "parses lambda abstractions" $ do
      test "x.x" $ Lambda () "x" (Name () "x")
      test "x .y . z .99" $ Lambda () "x" (Lambda () "y" (Lambda () "z" (I () 99)))
    it "parses applications" $ do
      test "(x.y.add ((mult x x)) y) 2 3" $ App () [Lambda () "x" (Lambda () "y" (App () [Name () "add", App () [Name () "mult", Name () "x", Name () "x"], Name () "y"])), I () 2, I () 3]
    it "parses nested aplications" $ do
      test "([x=1] x) ([x=1] 1 x)" $ App () [App () [LazyRecord () [("x", I () 1)], Name () "x"], App () [LazyRecord () [("x", I () 1)], I () 1, Name () "x"]]
