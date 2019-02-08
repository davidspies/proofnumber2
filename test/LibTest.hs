module LibTest where

import           DSpies.Prelude

import           Test.Hspec

spec_foo :: Spec
spec_foo = describe "Foo" $ it "should bar" $ (2 :: Integer) `shouldBe` 2
