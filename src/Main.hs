{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson

import Database.SQLite.Simple

import Happstack.Server

import System.Directory (removeFile)

import API
import Files
import Group
import Types
import User
import Util

-- | our main application
main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dir "user" userApi
    , dir "group" groupApi
    , dir "files" fileApi
    , e404
    ]

-- | our user-related API functions
userApi :: ServerPart Response
userApi = msum
    [ dir "new" newUser
    , dir "auth" getCreds
    , e404
    ]

-- | our group-related API functions
groupApi :: ServerPart Response
groupApi = msum
    [ dir "new" newGroup
    , dir "add" addUser
    , e404
    ]

-- | our file-related API functions
fileApi :: ServerPart Response
fileApi = msum
    [ dir "browse" browse
    , dir "pull" pullFile
    , dir "push" pushFile
    , e404
    ]

-- | create a new user
newUser :: ServerPart Response
newUser = apiTemplateIO (verifyPerms insertUser)

-- | add a group
newGroup :: ServerPart Response
newGroup = apiTemplateIO (verifyPerms insertGroup)

-- | get your key by providing username and password
getCreds :: ServerPart Response
getCreds = apiTemplateIO authUser

-- | add a user to a group
addUser :: ServerPart Response
addUser = apiTemplateIO (verifyPerms addUserToGroup)

-- | get all files' names
browse :: ServerPart Response
browse = apiTemplateIO (verifyPerms listFiles)

-- | pull a file's contents
pullFile :: ServerPart Response
pullFile = apiTemplateIO (verifyPerms getFile)

-- | push a file's contents
pushFile :: ServerPart Response
pushFile = apiTemplateIO (verifyPerms uploadFile)

-- | an API template used for regular data exchange via JSON
apiTemplate :: (ToJSON r, FromJSON i)
            => (i -> ServerPart r) -> ServerPart Response
apiTemplate re = do
    method POST
    decodeBody (defaultBodyPolicy "/tmp/" 0 65536 65536)
    rBody <- liftIO . takeRequestBody =<< askRq
    case unBody <$> rBody >>= decode of
      Just b -> re b >>= ok . respond
      Nothing -> ok $ respond (failure BadRequest :: ApiResponse ())

-- | a version of the API template above for IO actions 
apiTemplateIO :: (ToJSON r, FromJSON i) => (i -> IO r) -> ServerPart Response
apiTemplateIO re = apiTemplate (liftIO . re)

-- | verify the validity and integrity of a request
verifyPerms :: ApiRequest r
            => (r -> IO (ApiResponse a)) -> r -> IO (ApiResponse a)
verifyPerms action req = do
    errors <- verify req
    case errors of
      Just err ->  return $ failure err
      Nothing -> action req

-- | a temporary solution to determine a part of the API not yet implemented
e404 :: ServerPart Response
e404 = notFound $ toResponse ("404: Not here, stop searching!" :: String)

-- | initialize all database tables.
initDatabase :: IO ()
initDatabase = do
    removeFile "data/users.db"
    con <- open "data/users.db"
    execute_ con "CREATE TABLE users (id INTEGER PRIMARY KEY, \
                 \name TEXT, password TEXT, api_key TEXT)"
    execute_ con "CREATE TABLE groups (id INTEGER PRIMARY KEY, name TEXT)"
    execute_ con "CREATE TABLE membership (user_id INTEGER, group_id INTEGER)"
    execute_ con "CREATE TABLE user_ownership (user_id INTEGER, path TEXT)"
    execute_ con "CREATE TABLE group_ownership (group_id INTEGER, path TEXT)"

