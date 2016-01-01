module User where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Crypto.Scrypt

import Data.Acid (Query, Update, makeAcidic)
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.IxSet
import Data.Text (pack)

import System.Entropy

import Types
import Util

-- | store a user, updating it if told so
packUser :: Bool -> InternalUser -> Update Morgue InternalUser
packUser update user = do
    morgue <- get
    put $ morgue { allUsers = func user (allUsers morgue) }
    return user
    where func | update = updateIx (iUserName user)
               | otherwise = insert

-- | conversion for public access
toUser :: InternalUser -> User
toUser = User <$> iUserName <*> iApiKey

-- | provide data needed for signup. We don't do any IO here
signUpProvider :: SignUpRequest -> Query Morgue SignUpData
signUpProvider req = do
    morgue <- ask
    return (req, getOne $ allUsers morgue @= uName (suRqCreds req))

-- | process data from a sign up request
makeUser :: SignUpData -> ApiResponse InternalUser
makeUser (SignUpRequest creds apiKey salt, Nothing) = success $
    InternalUser (uName creds) apiKey encPass []
    where encPass = B.fromStrict . getEncryptedPass $
              encryptPass' salt (Pass . B.toStrict $ uPass creds)
makeUser (_, Just _) = failure EntityAlreadyExists

-- | store a user
storeUser :: InternalUser -> Update Morgue InternalUser
storeUser = packUser False

-- | provide data from a login request 
signInProvider :: SignInRequest -> Query Morgue SignInData
signInProvider (SignInRequest creds apiKey) = do
    morgue <- ask
    return ( uPass creds
           , getOne $ allUsers morgue @= uName creds
           , apiKey
           )

-- | process user data in order to login
loginUser :: SignInData -> ApiResponse InternalUser
loginUser (encPass, Just user@(InternalUser _ _ pass _), newApiKey)
    | verified = success $ user { iApiKey = newApiKey }
    | otherwise = failure AuthError
    where verified = verifyPass'
              (Pass $ B.toStrict pass)
              (EncryptedPass $ B.toStrict encPass)
loginUser (_, Nothing, _) = failure AuthError

-- | update a user
updateUser :: InternalUser -> Update Morgue InternalUser
updateUser = packUser True

-- | generate a user's API key
genApiKey :: IO ApiKey
genApiKey = pack . showDigest <$> (hmacSha256 <$>
    (B.fromStrict <$> getEntropy 256) <*>
    (B.fromStrict <$> getEntropy 256))

-- | transform a SignUpRequest as passed to our application to a version
-- suited for pure computation
toSignUpRequest :: SignUpRequest' -> IO SignUpRequest
toSignUpRequest req =
    SignUpRequest (suRqCreds' req) <$> genApiKey <*> newSalt

toSignInRequest :: SignInRequest' -> IO SignInRequest
toSignInRequest req =
    SignInRequest (siRqCreds' req) <$> genApiKey
