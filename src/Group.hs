module Group where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Data.Acid (Query, Update)
import Data.IxSet

import Types
import Util

-- = Utility functions

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

-- | conversion for public access
toGroup :: InternalGroup -> Group
toGroup = Group <$> iGroupName <*> iUsers

-- | conversion for public access
toGroupFileList :: InternalGroup -> GroupFileList
toGroupFileList = GroupFileList <$> iGroupName <*>
    (map fileName . iGroupFiles)

-- = Creating new groups
-- | provide data needed for group creation
groupNewProvider :: GroupNewRequest -> Query Morgue GroupNewData
groupNewProvider (GroupNewRequest user gName) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , gName
           , getOne $ allGroups morgue @= groupName
           )

-- | process all data from a new group request
makeGroup :: GroupNewData -> ApiResponse InternalGroup
makeGroup (Just user, gName, Nothing) = success $
    InternalGroup gName [iUserName user] []
makeGroup (Nothing, _, _) = failure AuthError
makeGroup (_, _, Just _) = failure GroupExists

-- = Adding users to groups
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
    | uName `elem` iUsers group = failure MemberExists
    | otherwise = success $ group { iUsers = uName : iUsers group }
addUserToGroup (Nothing, _, _) = failure AuthError
addUserToGroup (_, Nothing, _) = failure NoSuchGroup
addUserToGroup (_, _, Nothing) = failure NoSuchUser

-- = Pushing files
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
    | file `elem` iGroupFiles group = failure FileExists
    | otherwise = success $ group { iGroupFiles = file : iGroupFiles group }
addFileToGroup (Nothing, _, _) = failure AuthError
addFileToGroup (_, Nothing, _) = failure NoSuchGroup

-- = Pulling files
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
                    _ -> failure $ NoSuchFile fName
getFileFromGroup (Nothing, _, _) = failure AuthError
getFileFromGroup (_, Nothing, _) = failure NoSuchGroup

-- = patching files
-- | provide data from a patching request
patchGProvider :: PatchGRequest -> Query Morgue PatchGData
patchGProvider (PatchGRequest user gName fName patch) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , getOne $ allGroups morgue @= gName
           , fName
           , patch
           )

-- | process a patch in an impure fashion ;(
processGPatch :: PatchGData -> IO (ApiResponse InternalGroup)
processGPatch (Nothing, _, _, _) = return $ failure AuthError
processGPatch (_, Nothing, _, _) = return $ failure NoAccess
processGPatch (Just _, Just group@(InternalGroup _ _ gFiles), fName, patch) =
    case matchFiles gFiles [fName] of
      ApiResponse (Right [content]) -> do
          newFile <- patchFile (File fName content) patch
          return $ success group {
              iGroupFiles = replaceFile gFiles newFile }
      ApiResponse (Left err) -> return $ failure err
