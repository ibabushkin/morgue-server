{-# LANGUAGE OverloadedStrings #-}

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
class ApiRequest r where
    verify :: r -> IO (Maybe ApiError)
    verify r = do
        rr <- runVer r
        case rr of
          ApiResponse (Left err) -> return $ Just err
          ApiResponse (Right ()) -> return Nothing
    runVer :: r -> IO (ApiResponse ())

-- | users (for listing etc.)
instance ApiRequest User where
    runVer user = fromBool AuthError (verifyUser user)

-- | users (for creating new ones)
instance ApiRequest Credentials where
    runVer (Credentials uName _) =
        fromBool EntityAlreadyExists (isNothing <$> loadUser uName)

-- | pushing files
instance ApiRequest PushRequest where
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
instance ApiRequest FileRequest where
    runVer (FileRequest user fName) =
        (>>) <$> runVer user <*>
            fromBool NoAccess (user `mayAccess` fName)

-- | adding groups
instance ApiRequest GroupRequest where
    runVer (GroupRequest user gName) =
        (>>) <$> runVer user <*>
            fromBool EntityAlreadyExists
                (not <$> exists ["data", "g", getGName gName])

-- | adding users to groups
instance ApiRequest GroupAddRequest where
    runVer (GroupAddRequest user@(User uName _) group uName2) = do
        results <- sequence [ runVer user
                            , fromBool NoAccess
                                (isMemberIO uName (groupName group))
                            , fromBool NoSuchEntity (isJust <$> loadUser uName2)
                            , fromBool EntityAlreadyExists
                                (isMemberIO uName2 (groupName group))
                            ]
        return $ foldl1 (>>) results

-- | listing the users in a group
instance ApiRequest GroupListRequest where
    runVer (GroupListRequest user (Group gName)) = 
        (>>) <$> runVer user <*> fromBool NoSuchEntity
            (exists ["data", "g", getGName gName ++ ".json"])

-- | requesting processed content from morgue
instance ApiRequest ProcessingRequest where
    runVer (ProcessingRequest user _ fileNames) = do
        user <- runVer user 
        files <- mapM fileHelper fileNames
        return $ user >> foldl1 (>>) files
        where fileHelper fName = fromBool NoAccess (user `mayAccess` fName)
