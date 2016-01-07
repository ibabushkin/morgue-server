module Group where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Data.Acid (Query, Update)
import Data.IxSet

import Types
import Util

-- {{{ utility functions

-- | store a group, updating if told so
packGroup :: Bool -> InternalGroup -> Update Morgue InternalGroup
packGroup update group = do
    morgue <- get
    put $ morgue { allGroups = func group (allGroups morgue) }
    return group
    where func | update = updateIx (iGroupName group)
               | otherwise = insert

-- | store a new group
storeGroup :: InternalGroup -> Update Morgue InternalGroup
storeGroup = packGroup False

-- | update an existing group
updateGroup :: InternalGroup -> Update Morgue InternalGroup
updateGroup = packGroup True

toGroup :: InternalGroup -> Group
toGroup = Group <$> iGroupName <*> iUsers

toGroupFileList :: InternalGroup -> GroupFileList
toGroupFileList = GroupFileList <$> iGroupName <*>
    (map fileName . iGroupFiles)
-- }}}

-- {{{ creating new groups

-- | provide data needed for group creation
groupNewProvider :: GroupNewRequest -> Query Morgue GroupNewData
groupNewProvider (GroupNewRequest user groupName) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , groupName
           , getOne $ allGroups morgue @= groupName
           )

-- | process all data from a new group request
makeGroup :: GroupNewData -> ApiResponse InternalGroup
makeGroup (Just user, gName, Nothing) = success $
    InternalGroup gName [iUserName user] []
makeGroup (Nothing, _, _) = failure AuthError
makeGroup (_, _, Just _) = failure EntityAlreadyExists

-- }}}

-- {{{ adding users to groups

-- | providing data needed to add users to groups
groupAddProvider :: GroupAddRequest -> Query Morgue GroupAddData
groupAddProvider (GroupAddRequest user gName uName) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , getOne $ allGroups morgue @= gName
           , getOne $ allUsers morgue @= uName
           )

-- | process all data from a group addition request
addUserToGroup :: GroupAddData -> ApiResponse InternalGroup
addUserToGroup (Just user, Just group, Just (InternalUser uName _ _ _))
    | iUserName user `notElem` iUsers group = failure NoAccess
    | uName `elem` iUsers group = failure EntityAlreadyExists
    | otherwise = success $ group { iUsers = uName : iUsers group }
addUserToGroup (Nothing, _, _) = failure AuthError
addUserToGroup (_, Nothing, _) = failure NoSuchEntity
addUserToGroup (_, _, Nothing) = failure NoSuchEntity

-- }}}

-- {{{ pushing files

-- | providing data needed to push files
pushGProvider :: PushGRequest -> Query Morgue PushGData
pushGProvider (PushGRequest user gName file) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , getOne $ allGroups morgue @= gName
           , file
           )

-- | add a file to a group 
addFileToGroup :: PushGData -> ApiResponse InternalGroup
addFileToGroup (Just user, Just group, file)
    | iUserName user `notElem` iUsers group = failure NoAccess
    | file `elem` iGroupFiles group = failure EntityAlreadyExists
    | otherwise = success $ group { iGroupFiles = file : iGroupFiles group }
addFileToGroup (Nothing, _, _) = failure AuthError
addFileToGroup (_, Nothing, _) = failure NoSuchEntity

-- }}}

-- {{{ pulling files

-- | providing data needed to pull files
pullGProvider :: PullGRequest -> Query Morgue PullGData
pullGProvider (PullGRequest user gName fName) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , getOne $ allGroups morgue @= gName
           , fName
           )

-- | get a file from a group, looking it up by name
getFileFromGroup :: PullGData -> ApiResponse File
getFileFromGroup (Just user, Just group, fName)
    | iUserName user `notElem` iUsers group = failure NoAccess
    | otherwise = case filter ((==fName) . fileName) $ iGroupFiles group of
                    [file] -> success file
                    [] -> failure NoSuchEntity
getFileFromGroup (Nothing, _, _) = failure AuthError
getFileFromGroup (_, Nothing, _) = failure NoSuchEntity

-- }}}