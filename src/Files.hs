module Files where

import Data.Maybe (fromJust)
import qualified Data.Text as T

import Group
import Types
import Util

-- | an Owner can be determined to have access ("owning") a file(path)
class Owner o where
    owns :: o -> FileName -> Bool

-- | Users can own files exclusively
instance Owner UserName where
    uName `owns` fName = ["data", "u", getUName uName] == getFName fName

-- | Groups can own files collectively
instance Owner GroupName where
    gName `owns` fName = ["data", "u", getGName gName] == getFName fName

-- | get a file from the data store
loadFile :: FileName -> IO (Maybe File)
loadFile fName = do
    content <- loadText fName
    return $ File fName <$> content

storeFile :: File -> IO FileName
storeFile = undefined

uploadFile :: FileRequest -> IO FileName
uploadFile = undefined

listFiles :: User -> IO FileList
listFiles = undefined

-- | get a file from the data store, assumes verification up-front
getFile :: FileRequest -> IO File
getFile (FileRequest _ fName) = fromJust <$> loadFile fName

-- | check whether a user may access a file
mayAccess :: User -> FileName -> IO Bool
(User uName _) `mayAccess` fName = do
    uGroups <- map iGroupName <$> getGroups uName <$> listGroups
    return (uName `owns` fName || any (`owns` fName) uGroups)
