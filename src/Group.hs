module Group where

import Data.Maybe (isJust, fromJust, isNothing)

import Types
import User (verifyUser)
import Util

-- {{{ creation
-- | generate a new group
mkGroup :: GroupRequest -> Group
mkGroup (GroupRequest user gName) =
    Group gName [userName user]
-- }}}

-- {{{ interfacing with the datastore
-- | get a group from the data store
loadGroup :: GroupName -> IO (Maybe Group)
loadGroup (GroupName gName) = load $ FileName ["data", "g", gName ++ ".json"]

-- | store a group
storeGroup :: Group -> IO Group
storeGroup group = store (FileName
    ["data", "g", (++".json") . getGName $ iGroupName group]) group >>
        return group
-- }}}

-- {{{ manipulation primitives
-- | insert a user in a group
addUser :: Group -> UserName -> Group
addUser group uName = group { iUsers = uName : iUsers group }

-- | get all groups for a user
getGroups :: UserName -> [Group] -> [Group]
getGroups uName = filter (isMember uName)

-- | list all groups on the server
listGroups :: IO [Group]
listGroups = loadAll "data/g/"

-- | check for membership of a user in a group
isMember :: UserName -> Group -> Bool
isMember uName = (uName`elem`) . iUsers
-- }}}

-- {{{ API
-- | verify a group creation request
verifyGroupCreation :: GroupRequest
                    -> IO (ApiResponse GroupRequest GroupRequest)
verifyGroupCreation req@(GroupRequest user gName) = do
    userVer <- fromBoolIO verifyUser AuthError user
    gNameVer <- fromBoolIO
        ((isNothing<$>) . loadGroup) EntityAlreadyExists gName
    return $ userVer >> gNameVer >> resp
    where resp = success req :: ApiResponse GroupRequest GroupRequest

-- | verify a group addition
verifyGroupAddition :: GroupAddRequest
                    -> IO (ApiResponse GroupAddRequest GroupAddData)
verifyGroupAddition (GroupAddRequest user gName _) = do
    userVer <- fromBoolIO verifyUser AuthError user
    group <- loadGroup gName
    case group of
      Just g -> let memberVer = fromBool (isMember (userName user)) NoAccess g
                 in return $ userVer >> memberVer >> success (g, userName user)
      Nothing -> return $ failure NoSuchEntity

-- | process a group addition with error handling
processGroupAddition :: GroupAddData
                     -> ApiResponse GroupAddRequest Group
processGroupAddition (group, uName)
    | uName `elem` iUsers group  = failure EntityAlreadyExists
    | otherwise = success $ addUser group uName
-- }}}
