module User where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Crypto.Scrypt

import Data.Acid (Query, Update, makeAcidic)
import qualified Data.ByteString.Lazy as B
import Data.IxSet

import Types
import Util

-- | store a user, updating it if told so
packUser :: Bool -> InternalUser -> Update Morgue User
packUser update user = do
    morgue <- get
    put $ morgue { allUsers = func user (allUsers morgue) }
    return $ toUser user
    where func | update = updateIx (iUserName user)
               | otherwise = insert

-- | conversion for public access
toUser :: InternalUser -> User
toUser = User <$> iUserName <*> iApiKey

-- | provide data needed for signup. We don't do any IO here
signUpProvider :: SignUpRequest -> Query Morgue SignUpData
signUpProvider req = do
    morgue <- ask
    return $ (req, getOne $ (allUsers morgue) @= (uName $ suRqCreds req))

-- | process data from a sign up request
makeUser :: SignUpData -> ApiResponse SignUpRequest InternalUser
makeUser (SignUpRequest creds apiKey salt, Nothing) = success $
    InternalUser (uName creds) apiKey encPass []
    where encPass = B.fromStrict . getEncryptedPass $
              encryptPass' salt (Pass . B.toStrict $ uPass creds)
signUpProcessor (_, Just _) = failure EntityAlreadyExists

-- | store a user
storeUser :: InternalUser -> Update Morgue User
storeUser = packUser False

-- | provide data from a login request 
signInProvider :: SignInRequest -> Query Morgue SignInData
signInProvider (SignInRequest creds apiKey) = do
    morgue <- ask
    return $ ( uPass creds
             , getOne $ (allUsers morgue) @= (uName creds)
             , apiKey
             )

-- | process user data in order to login
loginUser :: SignInData -> ApiResponse SignInRequest InternalUser
loginUser (encPass, Just user@(InternalUser _ _ pass _), newApiKey)
    | verified = success $ user { iApiKey = newApiKey }
    | otherwise = failure AuthError
    where verified = verifyPass'
              (Pass $ B.toStrict pass)
              (EncryptedPass $ B.toStrict encPass)
loginUser (_, Nothing, _) = failure AuthError

-- | update a user
updateUser :: InternalUser -> Update Morgue User
updateUser = packUser True
