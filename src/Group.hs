module Group where

import Data.Maybe (isJust)

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
