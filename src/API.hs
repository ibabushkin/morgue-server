{-# LANGUAGE OverloadedStrings,
             FunctionalDependencies,
             FlexibleInstances
             #-}

module API where

import Crypto.Scrypt (Salt, newSalt)

import Data.Aeson ()
import Data.Maybe (isNothing, isJust)

import Happstack.Server ()

import Types
import Files
import Group
import Morgue
import User
import Util

{- | an API request. This is a bit more complicated compared to what I
usually write. An `ApiRequest`'s processing is sliced into layers, of
which there are three, represented by the functions `verify`, `process`,
and `finish`, respectively. The request gets transformed step-by-step.
An explanation of types present:
* `r` is the request itself, which is checked for appropriate permissions
  in stage one (`verify`).
* `i` is data needed to compute the result of a request, returned by `verify`
  and passed on to `process` (stages one and two).
* `process`, in turn returns a value of type `v` which is the result of the
  computation and which is passed to `finish` which is intended to store it,
  but might choose to transform it before returning, resulting in a value of
  type `v2`. However, each of those types is wrapped in a `ApiResponse` to
  allow for error handling etc.
-}
class ApiRequest r i v v2 | r -> i v v2 where
    -- | verify a request's integrity and gather data needed for `process`
    verify :: r -> IO (ApiResponse r i)
    -- | perform computation
    process :: i -> ApiResponse r v
    -- | do IO after computation
    finish :: ApiResponse r v -> IO (ApiResponse r v2)
    -- | run a request
    run :: r -> IO (ApiResponse r v2)
    run req = ((>>= process) <$> verify req) >>= finish

-- | requests to create a new user
instance ApiRequest SignUpRequest SignUpData InternalUser User where
    verify req = let a = fromBool (('/'`notElem`)
                                   . getUName
                                   . uName
                                   . suRqCreds) IllegalName req
                  in (a>>) <$> verifySignUp req
    process = mkUser
    finish = wrapFinish storeUser

-- | requests to sign in as a user
instance ApiRequest SignInRequest SignInData InternalUser User where
    verify = verifySignIn
    process = authUser
    finish = wrapFinish storeUser

-- | requests to create a new group
instance ApiRequest GroupRequest GroupRequest Group Group where 
    verify = verifyGroupCreation
    process = success . mkGroup
    finish = wrapFinish storeGroup

-- | requests to add a user to a group
instance ApiRequest GroupAddRequest GroupAddData Group Group where
    verify = verifyGroupAddition
    process = processGroupAddition
    finish = wrapFinish storeGroup

-- | requests to list all files available to a user
instance ApiRequest User FileListData [FileName] FileList where
    verify = wrapVerify verifyUser AuthError getUserAccess
    process (uName, gNames) = success $
        FileName ["data", "u", getUName uName] :
        map (FileName . ("data":("g":)) . (:[]) . getGName) gNames
    finish = undefined
    -- use the FileNames passed to build up Lists of file paths

-- | requests to pull a file
instance ApiRequest FileRequest File File File where
    verify = undefined
    process = undefined
    finish = undefined

-- | requests to push a file
instance ApiRequest PushRequest PushData File FileName where
    verify = undefined
    process = undefined
    finish = undefined
