{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import OntoLogic
import Parser (parseFOL, parseTerm, parseRegistry)

spec :: Spec
spec = do
  describe "FOL Parser" $ do
    it "parses constants" $ do
      parse parseTerm "" "\"hello\"" `shouldBe` Right (Constant "hello")

    it "parses numeric literals as strings" $ do
      parse parseTerm "" "123" `shouldBe` Right (Constant "123")
      parse parseTerm "" "45.67" `shouldBe` Right (Constant "45.67")

    it "parses variables" $ do
      parse parseTerm "" "x" `shouldBe` Right (Var "x")

    it "parses predicates" $ do
      parse parseFOL "" "IsRobot x" `shouldBe` 
        Right (Predicate "IsRobot" [Var "x"])

    it "parses nested logic (AND)" $ do
      parse parseFOL "" "and (A x) (B y)" `shouldBe`
        Right (And (Predicate "A" [Var "x"]) (Predicate "B" [Var "y"]))

    it "parses quantifiers (ForAll)" $ do
      parse parseFOL "" "forall (x) (P x)" `shouldBe`
        Right (ForAll "x" (Predicate "P" [Var "x"]))

  describe "Registry Parser" $ do
    it "parses a simple concept" $ do
      let input = "(def-concept Robot)"
      case parse parseRegistry "" input of
        Left err -> expectationFailure (errorBundlePretty err)
        Right reg -> concepts reg `shouldContain` [("Robot", Nothing)]

    it "parses a concept with parent" $ do
      let input = "(def-concept Robot Machine)"
      case parse parseRegistry "" input of
        Left err -> expectationFailure (errorBundlePretty err)
        Right reg -> concepts reg `shouldContain` [("Robot", Just "Machine")]
    
    it "parses a property" $ do
      let input = "(def-property is_active Machine Boolean)"
      case parse parseRegistry "" input of
        Left err -> expectationFailure (errorBundlePretty err)
        Right reg -> do
            let prop = head (properties reg)
            propName prop `shouldBe` "is_active"
            domain prop `shouldBe` "Machine"
            rangeType prop `shouldBe` "Boolean"
