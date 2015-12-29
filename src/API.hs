{-# LANGUAGE FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances
           , TypeFamilies
           , TemplateHaskell
           #-}

module API where

import Data.Acid

import Happstack.Server ()

import Types
import User
import Util

{- | an API request. This is a bit more complicated compared to what I
usually write. An `ApiRequest`'s processing is sliced into layers, of
which there are three, represented by the functions `provide`, `process`,
and `store`, respectively. The request gets transformed step-by-step.
An explanation of types present:
* `r` is the request itself, which is used to gather data needed for it's
  processing. This is done by `provide`.
* `i` is data needed to compute the result of a request, passed on to
  `process` (stage two).
* `process`, in turn, returns a value of type `v` which is the result of the
  computation and which is passed to `store` which is intended to store it,
  but might choose to transform it before returning, resulting in a value of
  type `v2`. However, each of those types is wrapped in a `ApiResponse` to
  allow for error handling etc.
-}
class ApiRequest r i v v2 | r -> i v v2, i -> r where
    provide :: r -> Query Morgue i
    process :: i -> ApiResponse r v
    store :: ApiResponse r v -> Update Morgue (ApiResponse r v2)

run :: ApiRequest r i v v2 => r -> Update Morgue (ApiResponse r v2)
run req = liftQuery (process <$> provide req) >>= store

-- | requests to sign up
instance ApiRequest SignUpRequest SignUpData InternalUser User where
    provide = signUpProvider
    process = makeUser
    store = liftStore storeUser

signUp :: SignUpRequest -> Update Morgue (ApiResponse SignUpRequest User)
signUp = run

-- | requests to sign in
instance ApiRequest SignInRequest SignInData InternalUser User where
    provide = signInProvider
    process = loginUser
    store = liftStore updateUser

signIn :: SignInRequest -> Update Morgue (ApiResponse SignInRequest User)
signIn = run

-- | derive IsAcidic instances
$(makeAcidic ''Morgue
  [ 'signUpProvider
  , 'signInProvider
  , 'packUser
  , 'signUp
  , 'signIn
  ])
