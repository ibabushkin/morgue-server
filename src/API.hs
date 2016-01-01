{-# LANGUAGE FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances
           , TypeFamilies
           , TemplateHaskell
           #-}

module API where

import Data.Acid

import Happstack.Server ()

import TemplateUtil
import Types
import User
import Util

-- {{{ synonyms for commonly used function types

-- | gather data from our datastore
type Provider r i = r -> Query Morgue i
-- | process said data
type Processor i r = i -> ApiResponse r
-- | store the data and optionally transform it to something else
type StorageBackend r' = ApiResponse r' -> Update Morgue (ApiResponse r')
-- | transform the result of an API call to something suited for the caller's eyes
type Transformer r' r = r' -> r
-- }}}

-- | process an API request by combining functions of the appropriate type
run :: Provider req int
    -> Processor int res'
    -> StorageBackend res'
    -> Transformer res' res
    -> req -> Update Morgue (ApiResponse res)
run provide process store trans req = fmap (fmap trans) $
    liftQuery (process <$> provide req) >>= store

-- {{{ boilerplate functions used for API calls, representing complete request pipelines

-- | signing up
signUp :: SignUpRequest -> Update Morgue (ApiResponse User)
signUp = run signUpProvider makeUser (liftStore storeUser) toUser

-- | logging in
signIn :: SignInRequest -> Update Morgue (ApiResponse User)
signIn = run signInProvider loginUser (liftStore updateUser) toUser

-- | derive IsAcidic instances
$(makeAcidic ''Morgue
  [ 'signUpProvider
  , 'signInProvider
  , 'packUser
  , 'signUp
  , 'signIn
  ])

actionIO :: ( UpdateEvent event
            , EventState event ~ Morgue
            , EventResult event ~ ApiResponse b)
         => AcidState Morgue
         -> (a -> event) -> a -> IO (ApiResponse b)
actionIO acid func = update acid . func
