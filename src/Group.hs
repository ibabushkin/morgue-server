module Group where

import Data.Maybe (isJust, fromJust)

import Types
import Util

-- | generate a new group
mkGroup :: GroupRequest -> InternalGroup
mkGroup (GroupRequest user gName) =
    InternalGroup gName [userName user]

-- | convert an internal group to a user
toGroup :: InternalGroup -> Group
toGroup = Group . iGroupName

-- | get a group from the data store
loadGroup :: GroupName -> IO (Maybe InternalGroup)
loadGroup (GroupName gName) = load ["data", "g", gName ++ ".json"]

-- | store a group
storeGroup :: InternalGroup -> IO InternalGroup
storeGroup group =
    store ["data", "g", getGName $ iGroupName group] group >> return group

-- | insert a new group
insertGroup :: GroupRequest -> IO InternalGroup
insertGroup gRequest = let group = mkGroup gRequest
                        in storeGroup group >> return group

-- | insert a user in a group
addUser :: InternalGroup -> UserName -> InternalGroup
addUser group uName = group { iUsers = uName : iUsers group }

addUserToGroup :: GroupAddRequest -> IO InternalGroup
addUserToGroup (GroupAddRequest _ (Group gName) uName) = do
    group <- fromJust <$> loadGroup gName
    let newGroup = addUser group uName
    storeGroup newGroup
    return newGroup

-- | get all groups for a user
getGroups :: UserName -> [InternalGroup] -> [InternalGroup]
getGroups uName = filter (isMember uName)

isMember :: UserName -> InternalGroup -> Bool
isMember uName = (uName`elem`) . iUsers

isMemberIO :: UserName -> GroupName -> IO Bool 
isMemberIO uName gName = do
    group <- loadGroup gName
    case group of
      Just (InternalGroup _ users) -> return $ uName `elem` users
      Nothing -> return False

listGroups :: IO [InternalGroup]
listGroups = loadAll "data/g/"
