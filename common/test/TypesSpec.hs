module TypesSpec
  ( spec,
  )
where

import qualified Test.Hspec as T
import Types

spec :: T.Spec
spec = T.describe "A User" $ do
  T.it "should be printable" (show (User 1 "me") `T.shouldBe` "[1] me")
