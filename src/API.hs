{-# LANGUAGE OverloadedStrings, FunctionalDependencies #-}

module API where

import Data.Aeson ()
import Data.Maybe (isNothing, isJust)

import Happstack.Server ()

import Types
import Files
import Group
import Morgue
import User
import Util

-- | wrap a check
fromBool :: ApiError -> IO Bool -> IO (ApiResponse ())
fromBool err res = do
    suc <- res
    if suc
       then return $ success ()
       else return $ failure err

-- | types that need to be verified when coming from an API call
class ApiRequest r p | r -> p where
    runVer :: r -> IO (ApiResponse p)

-- | users (for listing etc.)
-- TODO: implement new
instance ApiRequest User UserName where
    runVer user = fromBool AuthError (verifyUser user)

-- | users (for creating new ones)
-- TODO: implement new
instance ApiRequest Credentials Credentials where
    runVer (Credentials uName _) =
        fromBool EntityAlreadyExists (isNothing <$> loadUser uName)

-- | pushing files
-- TODO: implement new
-- take a push request and return a file with manipulated path
instance ApiRequest PushRequest File where
    runVer (PushRequest user Nothing (File fName _)) =
        (>>) <$> runVer user <*>
            fromBool EntityAlreadyExists (not <$> exists ("data":fName))
    runVer (PushRequest user (Just group) (File fName _)) = do
        results <- sequence [ runVer user
                            , fromBool NoAccess
                                 (isMemberIO (userName user) (groupName group))
                            , fromBool EntityAlreadyExists
                                 (exists ("data":fName))
                            ]
        return $ foldl1 (>>) results

-- | pulling files
-- TODO: implement new
instance ApiRequest FileRequest FileName where
    runVer (FileRequest user fName) =
        (>>) <$> runVer user <*>
            fromBool NoAccess (user `mayAccess` fName)

-- | adding groups
-- TODO: implement new
instance ApiRequest GroupRequest GroupRequest where
    runVer (GroupRequest user gName) =
        (>>) <$> runVer user <*>
            fromBool EntityAlreadyExists
                (not <$> exists ["data", "g", getGName gName])

-- | adding users to groups
-- TODO: implement new
instance ApiRequest GroupAddRequest UserName where
    runVer (GroupAddRequest user@(User uName _) group uName2) = do
        results <- sequence [ runVer user
                            , fromBool NoAccess
                                (isMemberIO uName (groupName group))
                            , fromBool NoSuchEntity (isJust <$> loadUser uName2)
                            , fromBool EntityAlreadyExists
                                (isMemberIO uName2 (groupName group))
                            ]
        return $ foldl1 (>>) results

-- | requesting processed content from morgue
-- TODO: implement new
instance ApiRequest ProcessingRequest ProcessingRequest where
    runVer (ProcessingRequest user _ fileNames) = do
        user <- runVer user 
        files <- mapM fileHelper fileNames
        return $ user >> foldl1 (>>) files
        where fileHelper fName = fromBool NoAccess (user `mayAccess` fName)
