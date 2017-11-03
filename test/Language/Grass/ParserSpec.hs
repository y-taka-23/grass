module Language.Grass.ParserSpec where

import Language.Grass.Parser
import Language.Grass.Types

import Test.Hspec

spec :: Spec
spec = do
    -- code ::= abs | code 'v' abs | code 'v' app*
    describe "parseGrass" $ do

        context "when you use ascii characters" $ do
            -- abs
            it "parses an Abs of no body" $ do
                parseGrass "" "ww" `shouldBe`
                    Right [Abs 2 []]
            it "parses an Abs with a single-App body" $ do
                parseGrass "" "wwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3]]
            it "parses an Abs with a mutli-App body" $ do
                parseGrass "" "wwWwwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3, App 1 3]]

            -- code 'v' abs
            it "parses an Abs preceded by a single Abs" $ do
                parseGrass "" "wwWwwwvwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], Abs 2 [App 1 3]]
            it "parses an Abs preceded by multiple Abs-es" $ do
                parseGrass "" "wwWwwwvwwWwwwvwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], Abs 2 [App 1 3], Abs 2 [App 1 3]]
            it "parses an Abs preceded by an empty App" $ do
                parseGrass "" "wwWwwwvvwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], Abs 2 [App 1 3]]
            it "parses an Abs preceded by a single App" $ do
                parseGrass "" "wwWwwwvWwwwvwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3, Abs 2 [App 1 3]]
            it "parses an Abs preceded by multiple Apps" $ do
                parseGrass "" "wwWwwwvWwwwWwwwvwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3, App 1 3, Abs 2 [App 1 3]]

            -- code 'v' app*
            it "parses an empty App preceded by a single Abs" $ do
                parseGrass "" "wwWwwwv" `shouldBe`
                    Right [Abs 2 [App 1 3]]
            it "parses an empty App preceded by multiple Abs-es" $ do
                parseGrass "" "wwWwwwvwwWwwwv" `shouldBe`
                    Right [Abs 2 [App 1 3], Abs 2 [App 1 3]]
            it "parses an empty App preceded by multiple Apps" $ do
                parseGrass "" "wwWwwwvWwwwWwwwv" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3, App 1 3]
            it "parses an single App preceded by a single Abs" $ do
                parseGrass "" "wwWwwwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "parses an single App preceded by multiple Abs-es" $ do
                parseGrass "" "wwWwwwvwwWwwwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], Abs 2 [App 1 3], App 1 3]
            it "parses multiple Apps preceded by multiple Apps" $ do
                parseGrass "" "wwWwwwvWwwwWwwwvWwwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3, App 1 3, App 1 3, App 1 3]
            it "parses multiple Apps preceded by a single Abs" $ do
                parseGrass "" "wwWwwwvWwwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3, App 1 3]
            it "parses multiple Apps preceded by multiple Abs-es" $ do
                parseGrass "" "wwWwwwvwwWwwwvWwwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], Abs 2 [App 1 3], App 1 3, App 1 3]
            it "parses multiple Apps preceded by multiple Apps" $ do
                parseGrass "" "wwWwwwvWwwwWwwwvWwwwWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3, App 1 3, App 1 3, App 1 3]

        context "when you use multi-byte characters" $ do
            it "parses a String which includes a mutli-byte 'ｗ'" $ do
                parseGrass "" "wwWｗwwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "parses a String which includes a mutli-byte 'Ｗ'" $ do
                parseGrass "" "wwＷwwwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "parses a String which includes a mutli-byte 'ｖ'" $ do
                parseGrass "" "wwWwwwｖWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]

        context "when you use comments" $ do
            it "ignores non-token ascci characters at the head of code" $ do
                parseGrass "" "XXwwWwwwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "ignores non-token ascci characters in the middle of code" $ do
                parseGrass "" "wwWwwXXwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "ignores non-token ascci characters at the tail of code" $ do
                parseGrass "" "wwWwwwvWwwwXX" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "ignores non-token multi-byte characters at the head of code" $ do
                parseGrass "" "ＸＸwwWwwwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "ignores non-token multi-byte characters in the middle of code" $ do
                parseGrass "" "wwWwwＸＸwvWwww" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
            it "ignores non-token multi-byte characters at the tail of code" $ do
                parseGrass "" "wwWwwwvWwwwＸＸ" `shouldBe`
                    Right [Abs 2 [App 1 3], App 1 3]
