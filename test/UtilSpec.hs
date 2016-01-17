module UtilSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import Util

import Default

-- | main spec
spec :: Spec
spec = describe "Util" $ do
    fileMatchSpec
    groupMatchSpec
    fileReplaceSpec

-- = specs
-- | matchFile spec
fileMatchSpec :: Spec
fileMatchSpec = describe "Util.matchFiles" $ do
    it "handles empty queries and file lists correctly" $
        matchFiles [] [] == success []
    it "handles empty queries correctly" $ property $
        \fs -> matchFiles fs [] == success []
    it "handles empty file lists correctly" $ property $
        \(NonEmpty fs@(f:fss)) -> matchFiles [] fs == failure (NoSuchFile f)
    it "processes all input given unique file data" $ property $
        forAll ((,) <$> uniqueFileGen <*> arbitrary) $
        \(fs, Positive n) -> n < length fs ==>
            let files = take n fs
             in matchFiles fs (map fileName files)
                    == success (map fileContents files)

-- | matchGroup spec
groupMatchSpec :: Spec
groupMatchSpec = describe "Util.matchGroups" $ do
    it "handles empty queries and group lists correctly" $
        matchGroups [] [] == success []
    it "handles empty queries correctly" $ property $
        \gs -> matchGroups gs [] == success []
    it "handles empty group lists correctly" $ property $
        \(NonEmpty gfs) -> matchGroups [] gfs == failure NoAccess
    it "handles normal input correctly" $ pendingWith "think about it"

-- | replaceFile spec
fileReplaceSpec :: Spec
fileReplaceSpec = describe "Util.replaceFile" $ do
    it "handles empty file lists" $ property $
        \f -> replaceFile [] f == []
    it "doesn't touch other files" $ property $
        \(fs, f) -> f `notElem` fs ==> replaceFile fs f == fs
    it "honours invariants: length" $ property $
        \(fs, f) -> length (replaceFile fs f) == length fs
