module GroupSpec where

import Test.QuickCheck
import Test.Hspec

import Types
import Group

-- | main spec
spec :: Spec
spec = describe "Group" $ do
    makeGroupSpec

makeGroupSpec :: Spec
makeGroupSpec = describe "Group.makeGroup" $ do
    it "throws errors appropriately" $
        pendingWith "use quickcheck"
