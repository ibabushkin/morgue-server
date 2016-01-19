module GroupSpec where

import Test.QuickCheck
import Test.Hspec

import Types
import Group

-- | main spec
spec :: Spec
spec = describe "Group" $
    makeGroupSpec

makeGroupSpec :: Spec
makeGroupSpec = describe "Group.makeGroup" $
    it "throws errors appropriately" $
        pendingWith "use quickcheck"
