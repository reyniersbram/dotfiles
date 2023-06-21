module UtilsTest (
    joinTest
) where 

import Test.Hspec

import Utils (join)

joinTest :: IO ()
joinTest = hspec $ do
    it "Test join" $ do
        join ["a", "b", "c"] " " `shouldBe` "a b c"
    it "Test join single" $ do
        join ["a"] " " `shouldBe` "a"
    it "Test join empty" $ do
        join [] " " `shouldBe` ""
    it "Test join sep" $ do
        join ["a", "b", "c"] "%" `shouldBe` "a%b%c"
