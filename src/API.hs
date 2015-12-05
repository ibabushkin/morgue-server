{-# LANGUAGE OverloadedStrings #-}

module API where

import Data.Aeson ()

import Database.SQLite.Simple ()

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
    runVer user@(User uName _) = fromBool AuthError
        ((user `elem`) <$> getUser uName)

-- | users (for creating new ones)
instance ApiRequest Credentials where
    runVer (Credentials uName _) =
        fromBool EntityAlreadyExists (not <$> existsUser uName)

-- | pushing files
instance ApiRequest PushRequest where
    runVer (PushRequest user Nothing f) =
        (>>) <$> runVer user <*>
            fromBool EntityAlreadyExists (not <$> fileExisting f user)
    runVer (PushRequest user (Just group) file) = do
        results <- sequence [ runVer user
                            , fromBool NoAccess (isMember user group)
                            , fromBool EntityAlreadyExists
                                 (fileExisting file group)
                            ]
        return $ foldl1 (>>) results

-- | pulling files
instance ApiRequest FileRequest where
    runVer (FileRequest user filename) =
        (>>) <$> runVer user <*>
            fromBool NoAccess (checkPermissions ("data/" ++ filename) user)

-- | adding groups
instance ApiRequest GroupRequest where
    runVer (GroupRequest user gName) =
        (>>) <$> runVer user <*>
            fromBool EntityAlreadyExists (existsGroup gName)

-- | adding users to groups
instance ApiRequest GroupAddRequest where
    runVer (GroupAddRequest user@(User uName _) group uName2) = do
        results <- sequence [ runVer user
                            , fromBool NoAccess (isMember user group)
                            , fromBool NoSuchEntity (existsUser uName)
                            , user2helper
                            ]
        return $ foldl1 (>>) results
        where user2helper = do
                  [user2] <- getUser uName2
                  fromBool EntityAlreadyExists (not <$> isMember user2 group)

-- | requesting processed content from morgue
instance ApiRequest ProcessingRequest where
    runVer (ProcessingRequest user _ fileNames) = do
        user <- runVer user 
        files <- mapM fileHelper fileNames
        return $ user >> foldl1 (>>) files
        where fileHelper fName = fromBool NoAccess
                  (checkPermissions ("data/" ++ fName) user)
