{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson

import Happstack.Server

import API
import Files
import Group
import Morgue
import Types
import User
import Util

-- | our main application
main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dir "user" userApi
    , dir "group" groupApi
    , dir "files" fileApi
    , dirGen "morgue" getAgenda
    , e404
    ]

-- | our user-related API functions
userApi :: ServerPart Response
userApi = msum
    [ dirGen "new" (mkUser >>= storeUser)
    , dirGen "auth" (authUser >>= storeUser)
    , e404
    ]

-- | our group-related API functions
groupApi :: ServerPart Response
groupApi = msum
    [ dirGen "new" storeGroup
    , dirGen "add" addUserToGroup
    , e404
    ]

-- | our file-related API functions
fileApi :: ServerPart Response
fileApi = msum
    [ dirGen "browse" listFiles
    , dirGen "pull" getFile
    , dirGen "push" uploadFile
    , e404
    ]

-- | a wrapper around Happstack's `dir` to remove boilerplate
-- from the generic functions below
dirGen :: (ToJSON a, FromJSON r, ApiRequest r)
            => String -> (r -> IO (ApiResponse a)) -> ServerPart Response
dirGen s resp = dir s (apiGenIO resp)

-- | an API call used for regular data exchange via JSON
apiGen :: (ToJSON r, FromJSON i)
            => (i -> ServerPart r) -> ServerPart Response
apiGen re = do
    method POST
    decodeBody (defaultBodyPolicy "/tmp/" 0 65536 65536)
    rBody <- liftIO . takeRequestBody =<< askRq
    case unBody <$> rBody >>= decode of
      Just b -> re b >>= ok . respond
      Nothing -> ok $ respond (failure BadRequest :: ApiResponse ())

-- | a version of the API call above for IO actions, includes verification
apiGenIO :: (ToJSON a, FromJSON r, ApiRequest r)
              => (r -> IO (ApiResponse a)) -> ServerPart Response
apiGenIO re = apiGen (liftIO . verifyPerms re)

-- | verify the validity and integrity of a request
verifyPerms :: ApiRequest r
            => (r -> IO (ApiResponse a)) -> r -> IO (ApiResponse a)
verifyPerms action req = do
    errors <- verify req
    case errors of
      Just err ->  return $ failure err
      Nothing -> action req

-- | Docs Not Found.
e404 :: ServerPart Response
e404 = notFound $ toResponse ("404: Not here, stop searching!" :: String)
