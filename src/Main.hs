{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Control.Monad (msum, (>=>))
import Control.Monad.IO.Class (liftIO)

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Aeson

import Happstack.Server

import API
import Types
import User (toSignUpRequest, toSignInRequest, toProcessingRequest)
import Util

-- | our main application
main :: IO ()
main =
    bracket (openLocalState initialMorgueState)
            createCheckpointAndClose
            (simpleHTTP nullConf . route)

-- | our main application
route :: AcidState Morgue -> ServerPart Response
route acid = msum
    [ dir "user" $ userApi acid
    , dir "group" $ groupApi acid
    , e404
    ]

-- | our user-related API functions
userApi :: AcidState Morgue -> ServerPart Response
userApi acid = msum
    [ dirGen "new"  $ toSignUpRequest >=> actionIO acid SignUp
    , dirGen "auth" $ toSignInRequest >=> actionIO acid SignIn
    , dirGen "push" $ actionIO acid PushU
    , dirGen "pull" $ actionIO acid PullU
    , dirGen "list" $ actionIO acid List
    , dirGen "agenda" $ toProcessingRequest >=> actionIO acid Processing
    , e404
    ]

groupApi :: AcidState Morgue -> ServerPart Response
groupApi acid = msum
   [ dirGen "new"  $ actionIO acid GroupNew
   , dirGen "add"  $ actionIO acid GroupAdd
   , dirGen "push" $ actionIO acid PushG
   , dirGen "pull" $ actionIO acid PullG
   , e404
   ]

-- | a wrapper around Happstack's `dir` to remove boilerplate
-- from the generic functions below
dirGen :: (ToJSON a, FromJSON r)
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
apiGenIO :: (ToJSON a, FromJSON r)
         => (r -> IO (ApiResponse a)) -> ServerPart Response
apiGenIO re = apiGen (liftIO . re)

-- | Docs Not Found.
e404 :: ServerPart Response
e404 = notFound $ toResponse ("404: Not here, stop searching!" :: String)
