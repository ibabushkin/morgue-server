module Files where

import qualified Data.Text as T

import Group
import Types
import Util

-- | an Owner can be determined to have access ("owning") a file(path)
class Owner o where
    owns :: o -> FileName -> Bool

-- | Users can own files exclusively
instance Owner UserName where
    uName `owns` fName = ["data", "u", getUName uName] == fName

-- | Groups can own files collectively
instance Owner GroupName where
    gName `owns` fName = ["data", "u", getGName gName] == fName

-- | get a file from the data store
loadFile :: FileName -> IO (Maybe File)
loadFile fName = do
    content <- loadText fName
    return $ File fName <$> content

-- | check whether a user may access a file
mayAccess :: User -> FileName -> IO Bool
(User uName _) `mayAccess` fName = do
    uGroups <- (map iGroupName) <$> getGroups uName <$> listGroups
    return (uName `owns` fName || any (`owns` fName) uGroups)
