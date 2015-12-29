{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Aeson

import Happstack.Server

import API
import Types
import Util

-- | our main application
main :: IO ()
main = do
    bracket (openLocalState initialMorgueState)
            (createCheckpointAndClose)
            (simpleHTTP nullConf . route)

route :: AcidState Morgue -> ServerPart Response
route acid = msum
    [ dir "user" userApi
    --, dir "group" groupApi
    --, dir "files" fileApi
    --, dirGen "morgue" getAgenda
    , e404
    ]

-- | our user-related API functions
userApi :: ServerPart Response
userApi = msum
    {-[ dirGen "new" (run :: ApiCall SignUpRequest User)
    , dirGen "auth" (run :: ApiCall SignInRequest User)
    , e404-}
    []

{-
-- | our group-related API functions
groupApi :: ServerPart Response
groupApi = msum
    [ dirGen "new" (run :: ApiCall GroupRequest Group)
    , dirGen "add" (run :: ApiCall GroupAddRequest Group)
    , e404
    ]

-- | our file-related API functions
fileApi :: ServerPart Response
fileApi = msum
    [ dirGen "browse" (run :: ApiCall User FileList)
    , dirGen "pull" (run :: ApiCall FileRequest File)
    , dirGen "push" (run :: ApiCall PushRequest FileName)
    , e404
    ]

-- | a wrapper around Happstack's `dir` to remove boilerplate
-- from the generic functions below
dirGen :: (ToJSON a, FromJSON r, ApiRequest r b c a)
       => String
       -> (r -> Update Morgue (ApiResponse r a))
       -> ServerPart Response
dirGen s resp = undefined
      -}

-- | an API call used for regular data exchange via JSON
apiGen :: (ToJSON r, FromJSON i)
            => (i -> IO j) -> (j -> ServerPart r) -> ServerPart Response
apiGen get fun = do
    method POST
    decodeBody (defaultBodyPolicy "/tmp/" 0 65536 65536)
    rBody <- liftIO . takeRequestBody =<< askRq
    case unBody <$> rBody >>= decode of
      Just b -> do a <- liftIO $ get b
                   fun a >>= ok . respond
      Nothing -> ok $ respond (failure BadRequest :: ApiResponse i ())

-- | Docs Not Found.
e404 :: ServerPart Response
e404 = notFound $ toResponse ("404: Not here, stop searching!" :: String)
