module Language.Grass.EncoderSpec where

import Language.Grass.Encoder

import Test.Hspec
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck

spec :: Spec
spec = do
    describe "encode/decode" $ do
        prop "should be isomorphic" prop_isomorphic

prop_isomorphic :: Char -> Bool
prop_isomorphic ch = decode (encode ch) == Just ch
