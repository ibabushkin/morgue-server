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
import User ( toSignUpRequest, toSignInRequest
            , toProcessingRequest, processUPatch)
import Group (processGPatch)
import Util

-- | main application
main :: IO ()
main = bracket (openLocalState initialMorgueState)
    createCheckpointAndClose (simpleHTTP nullConf . route)

-- | main application's route
route :: AcidState Morgue -> ServerPart Response
route acid = msum
    [ dir "user" $ userApi acid
    , dir "group" $ groupApi acid
    , e404
    ]

-- | user-related API functions
userApi :: AcidState Morgue -> ServerPart Response
userApi acid = msum
    [ dirGen "new"  $ toSignUpRequest >=> actionIO acid SignUp
    , dirGen "auth" $ toSignInRequest >=> actionIO acid SignIn
    , dir "file" $ msum
        [ dirGen "push" $ actionIO acid PushU
        , dirGen "delete" $ actionIO acid DeleteU
        , dirGen "pull" $ actionIO acid PullU
        , dirGen "patch" $ externalActionIO acid
            PatchUProvider processUPatch UpdateUser
        , e404
        ]
    , dirGen "list" $ actionIO acid List
    , dirGen "agenda" $ toProcessingRequest >=> actionIO acid Processing
    , e404
    ]

-- | group-related API functions
groupApi :: AcidState Morgue -> ServerPart Response
groupApi acid = msum
   [ dirGen "new"  $ actionIO acid GroupNew
   , dirGen "add"  $ actionIO acid GroupAdd
   , dir "file" $ msum
       [ dirGen "push" $ actionIO acid PushG
       , dirGen "delete" $ actionIO acid DeleteG
       , dirGen "pull" $ actionIO acid PullG
       , dirGen "patch" $ externalActionIO acid
               PatchGProvider processGPatch UpdateGroup
       , e404
       ]
   , e404
   ]

-- | a wrapper around Happstack's `dir` to remove boilerplate
-- from the generic functions below
dirGen :: (ToJSON a, FromJSON r)
       => String -> (r -> IO (ApiResponse a)) -> ServerPart Response
dirGen s respAction = dir s (apiGenIO respAction)

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
