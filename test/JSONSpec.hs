{-# LANGUAGE OverloadedStrings #-}

module JSONSpec where

import Test.Hspec

import Types

spec :: Spec
spec = describe "Types.FromJSON" $ do
    modeSpec
    formatSpec
    optionsSpec

modeSpec :: Spec
modeSpec = describe "Types.FromJSON AgendaMode" $
    it "fails on wrong input" $ do
        (fromJSON (String "abd") :: Result AgendaMode)
        `shouldBe` (Error "mempty")

formatSpec :: Spec
formatSpec = describe "Types.FromJSON OutputFormat" $
    it "fails on wrong input" $ do
        (fromJSON (String "abd") :: Result OutputFormat)
        `shouldBe` (Error "mempty")

optionsSpec :: Spec
optionsSpec = describe "Types.FromJSON Options" $
    it "parses correctly" $ do
        (decode "{\"mode\":\"Todo\",\"double_spaces\":false,\
                \ \"tags\":[\"abc\"],\"num_days\":3,\"format\":\"ANSI\"}"
                :: Maybe Options)
        `shouldBe` (Just (AgendaOptions Todo False
                       (Just ["abc"]) Nothing 3 putStrLn ANSI))

