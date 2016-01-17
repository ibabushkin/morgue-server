{-# LANGUAGE OverloadedStrings #-}
module Default where

import Test.QuickCheck
import Test.QuickCheck.Instances

import Types

-- = Generators for our datatypes

instance Arbitrary UserName where
    arbitrary = UserName <$> arbitrary

instance Arbitrary InternalUser where
    arbitrary = InternalUser <$> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GroupName where
    arbitrary = GroupName <$> arbitrary

instance Arbitrary InternalGroup where
    arbitrary = InternalGroup <$> arbitrary <*>
        arbitrary <*> arbitrary

instance Arbitrary FileName where
    arbitrary = FileName <$> arbitrary

instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary

instance Arbitrary GroupFileList where
    arbitrary = GroupFileList <$> arbitrary <*> arbitrary

instance Arbitrary FileList where
    arbitrary = FileList <$> arbitrary <*> arbitrary

uniqueFileGen :: Gen [File]
uniqueFileGen = foldr add [] <$> (arbitrary :: Gen [File])
    where add a as | a `elem` as = as
                   | otherwise   = a : as
