module ExampleSpec (spec) where

import Qtility
import Test.Hspec

spec :: Spec
spec = do
  describe "`True` is `True`" $ do
    it "is so" $ do
      True `shouldBe` True
