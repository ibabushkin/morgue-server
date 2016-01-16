{-# LANGUAGE OverloadedStrings #-}
module UtilSpec where

import Test.Hspec

import Types
import Util

-- | main spec
spec :: Spec
spec = describe "Util" $ do
    fileMatchSpec
    groupMatchSpec
    fileReplaceSpec

-- = default values
defaultUserName = FileName "name"

defaultFileName = FileName "name"
defaultFileName2 = FileName "name2"
defaultContent = "def"
defaultFile = File defaultFileName defaultContent
defaultFile2 = File defaultFileName2 defaultContent

defaultGroupName = GroupName "name"
defaultGroup = InternalGroup defaultGroupName [] [defaultFile]
defaultGroup2 = InternalGroup defaultGroupName [] [defaultFile2]
defaultGFList = GroupFileList defaultGroupName [defaultFileName]

-- = specs
-- | matchFile spec
fileMatchSpec :: Spec
fileMatchSpec = describe "Util.matchFiles" $ do
    it "handles empty queries and file lists correctly" $
        matchFiles [] [] `shouldBe` success []
    it "handles empty queries correctly" $
        matchFiles [defaultFile] [] `shouldBe` success []
    it "handles empty file lists correctly" $
        matchFiles [] [defaultFileName] `shouldBe`
        failure (NoSuchFile defaultFileName)
    it "handles normal input correctly" $
        matchFiles [defaultFile] [defaultFileName] `shouldBe`
        success [defaultContent]

-- | matchGroup spec
groupMatchSpec :: Spec
groupMatchSpec = describe "Util.matchGroups" $ do
    it "handles empty queries and group lists correctly" $
        matchGroups [] [] `shouldBe` success []
    it "handles empty queries correctly" $
        matchGroups [defaultGroup] [] `shouldBe` success []
    it "handles empty group lists correctly" $
        matchGroups [] [defaultGFList] `shouldBe` failure NoAccess
    it "handles empty illegal input correctly" $
        matchGroups [defaultGroup2] [defaultGFList] `shouldBe`
        success [([defaultFile2], [defaultFileName])]
    it "handles normal input correctly" $
        matchGroups [defaultGroup] [defaultGFList] `shouldBe`
        success [([defaultFile], [defaultFileName])]

-- | replaceFile spec
fileReplaceSpec :: Spec
fileReplaceSpec = describe "Util.replaceFile" $ do
    it "handles empty file lists" $
        replaceFile [] defaultFile `shouldBe` []
    it "doesn't touch other files" $
        replaceFile [defaultFile] defaultFile2 `shouldBe` [defaultFile]
    it "replaces files passed" $
        replaceFile [defaultFile] defaultFile `shouldBe` [defaultFile]
    it "honours invariants: length" $
        replaceFile [defaultFile] defaultFile `shouldSatisfy` (==1) . length
        -- TODO: use quickcheck here
