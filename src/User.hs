{-# LANGUAGE TupleSections #-}

module User where

import Crypto.Scrypt

import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.Maybe (fromMaybe, isJust)

import System.Entropy

import Types
import Util

-- {{{ creation
-- | generate a new user
mkUser :: SignUpData -> ApiResponse SignUpRequest InternalUser
mkUser ((Credentials uName uPass), apiKey, salt) =
    success $ InternalUser uName apiKey
        ((B.fromStrict . getEncryptedPass)
        (encryptPass' salt (Pass $ B.toStrict uPass)))

-- | generate a user's API key
genApiKey :: IO ApiKey
genApiKey = showDigest <$> (hmacSha256 <$>
    (B.fromStrict <$> getEntropy 256) <*>
    (B.fromStrict <$> getEntropy 256))
-- }}}

-- {{{ interfacing with the datastore
-- | get a user from the data store
loadUser :: UserName -> IO (Maybe InternalUser)
loadUser (UserName uName) = load (FileName ["data", "u", uName ++ ".json"])

-- | store a new user
storeUser :: InternalUser -> IO User
storeUser user = do
    store (FileName ["data", "u", (++".json") . getUName $ iUserName user]) user
    return $ toUser user

-- | check the existance of a user
existsUser :: UserName -> IO Bool
existsUser uName = isJust <$> loadUser uName

-- | verify User
verifyUser :: User -> IO Bool
verifyUser user@(User uName _) =
    maybe False ((==user) . toUser) <$> loadUser uName
-- }}}

-- {{{ manipulation primitives
-- | convert an internal user to a user
toUser :: InternalUser -> User
toUser = User <$> iUserName <*> iApiKey
-- }}}

-- {{{ API
-- | signup verification
verifySignUp :: SignUpRequest -> IO (ApiResponse SignUpRequest SignUpData)
verifySignUp = wrapVerify (fmap (not<$>) existsUser . uName . suRqCreds)
    EntityAlreadyExists getter
    where getter req = (suRqCreds req,,) <$> genApiKey <*> newSalt

-- | signin verification
verifySignIn :: SignInRequest -> IO (ApiResponse SignInRequest SignInData)
verifySignIn req = do
    mUser <- loadUser . uName . siRqCreds $ req
    case mUser of
      Just user -> success <$>
          (uPass $ siRqCreds req, user,) <$> genApiKey
      Nothing -> return $ failure NoSuchEntity -- or AuthError

-- | authenticate a User
authUser :: SignInData -> ApiResponse r InternalUser
authUser (pass, u@(InternalUser uName' _ encPass), key)
    | res = success u { iApiKey = key }
    | otherwise = failure AuthError
    where res = verifyPass'
              (Pass $ B.toStrict pass)
              (EncryptedPass $ B.toStrict encPass)
-- }}}
