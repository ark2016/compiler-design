{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Parser (parse)
import AST (Program, Articles, Body, Instr(..))

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parses sample input with define abs" $ do
      let input = "define abs dup 0 < if -1 * endif end 10 abs -10 abs"
          expected :: Program
          expected =
            ( [ ("abs", [ IWord "dup"
                        , INum 0
                        , IWord "<"
                        , IIf [INum (-1), IWord "*"] Nothing
                        ])
              ]
            , [ INum 10
              , IWord "abs"
              , INum (-10)
              , IWord "abs"
              ]
            )
      parse input `shouldBe` Just expected

    it "parses empty input" $ do
      let input = ""
          expected = ([], [])
      parse input `shouldBe` Just expected

    it "parses simple instructions" $ do
      let input = "dup 10 swap"
          expected = ([], [IWord "dup", INum 10, IWord "swap"])
      parse input `shouldBe` Just expected

    it "parses if with else" $ do
      let input = "if 1 else 2 endif"
          expected = ([], [IIf [INum 1] (Just [INum 2])])
      parse input `shouldBe` Just expected

    it "parses nested if" $ do
      let input = "if if 1 endif endif"
          expected = ([], [IIf [IIf [INum 1] Nothing] Nothing])
      parse input `shouldBe` Just expected

    it "parses article definition and main program" $ do
      let input = "define inc 1 + end 5 inc"
          expected = ( [("inc", [INum 1, IWord "+"])],
                       [INum 5, IWord "inc"] )
      parse input `shouldBe` Just expected

    it "returns Nothing on missing end in article" $ do
      let input = "define foo dup"
      parse input `shouldBe` Nothing

    it "returns Nothing on unexpected endif" $ do
      let input = "dup endif"
      parse input `shouldBe` Nothing
