{-# LANGUAGE FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances
           , TypeFamilies
           , TemplateHaskell
           #-}

module API where

import Data.Acid

import Happstack.Server ()

import Group
import Types
import User
import Util

-- = Synonyms for commonly used function types

-- | gather data from our datastore
type Provider r i = r -> Query Morgue i
-- | process said data
type Processor i r = i -> ApiResponse r
-- | store the data and optionally transform it to something else
type StorageBackend r' = ApiResponse r' -> Update Morgue (ApiResponse r')
-- | transform the result of an API call to something suited for the caller's eyes
type Transformer r' r = r' -> r

-- = Combinator(s) for API building
-- | process an API request by combining functions of the appropriate type
run :: Provider req int
    -> Processor int res'
    -> StorageBackend res'
    -> Transformer res' res
    -> req -> Update Morgue (ApiResponse res)
run provide process store trans req = fmap (fmap trans) $
    liftQuery (process <$> provide req) >>= store

-- = Boilerplate functions representing complete request pipelines
-- | signing up
signUp :: SignUpRequest -> Update Morgue (ApiResponse User)
signUp = run signUpProvider makeUser (liftStore storeUser) toUser

-- | logging in
signIn :: SignInRequest -> Update Morgue (ApiResponse User)
signIn = run signInProvider loginUser (liftStore updateUser) toUser

-- | pushing files to a user's datastore
pushU :: PushURequest -> Update Morgue (ApiResponse FileName)
pushU = run pushUProvider addFileToUser (liftStore updateUser) getLastFile

-- | pulling files from a user's datastore
pullU :: PullURequest -> Update Morgue (ApiResponse File)
pullU = run pullUProvider getFileFromUser return id

-- | creating a new group
groupNew :: GroupNewRequest -> Update Morgue (ApiResponse Group)
groupNew = run groupNewProvider makeGroup (liftStore storeGroup) toGroup

-- | adding a user to an existing group
groupAdd :: GroupAddRequest -> Update Morgue (ApiResponse Group)
groupAdd = run groupAddProvider
    addUserToGroup (liftStore updateGroup) toGroup

-- | pushing files to a group's datastore
pushG :: PushGRequest -> Update Morgue (ApiResponse Group)
pushG = run pushGProvider addFileToGroup (liftStore updateGroup) toGroup

-- | pulling files from a group's datastore
pullG :: PullGRequest -> Update Morgue (ApiResponse File)
pullG = run pullGProvider getFileFromGroup return id

-- | listing files available to a user
list :: ListRequest -> Update Morgue (ApiResponse FileList)
list = run listProvider toFileList  return id

-- | processing a set of files to an agenda or outline
processing :: ProcessingRequest -> Update Morgue (ApiResponse String)
processing = run processingProvider processFiles return id

-- = API interface
-- | derive IsAcidic instances
$(makeAcidic ''Morgue
  [ 'signUp
  , 'signIn
  , 'pushU
  , 'pullU
  , 'groupNew
  , 'groupAdd
  , 'pushG
  , 'pullG
  , 'list
  , 'processing
  ])

-- | combinator to wrap request pipelines into IO actions
actionIO :: ( UpdateEvent event
            , EventState event ~ Morgue
            , EventResult event ~ ApiResponse b)
         => AcidState Morgue
         -> (a -> event) -> a -> IO (ApiResponse b)
actionIO acid func = update acid . func
