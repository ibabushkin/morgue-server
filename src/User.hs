{-# LANGUAGE OverloadedStrings #-}

module User where

import Crypto.Scrypt

import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.SHA (hmacSha256, showDigest)

import Database.SQLite.Simple

import System.Directory (createDirectoryIfMissing)
import System.Entropy (getEntropy)

import Types
import Util

-- | generate an API key from the system's RNG
generateKey :: IO ApiKey
generateKey = showDigest <$> (hmacSha256 <$>
    (L.fromStrict <$> getEntropy 256) <*> (L.fromStrict <$> getEntropy 256))

-- | does a user with that name exist?
existsUser :: UserName -> IO Bool
existsUser uName = ((==1) . length) <$> getUser uName

-- | get a list of users with only the given name
getUser :: UserName -> IO [User]
getUser uName = do
    con <- open "data/users.db"
    map (uncurry User) <$>
        (query con "SELECT name, api_key FROM users WHERE name=?"
            (Only uName) :: IO [(String, String)])

-- | Take a username and password, check whether a user can be created, 
-- hash the password with scrypt and derive an API key
insertUser :: Credentials -> IO (ApiResponse User)
insertUser (Credentials uName pass) = do
    con <- open "data/users.db"
    enc <- encryptPassIO defaultParams . Pass $ pack pass
    key <- generateKey
    execute con "INSERT INTO users (name, password, api_key) VALUES (?, ?, ?)"
       (uName, getEncryptedPass enc, key)
    createDirectoryIfMissing True ("data/" ++ uName)
    return . success $ User uName key

-- | authenticate a user against the DB, return the appropriate (new)
-- API key if succesful
authUser :: Credentials -> IO (ApiResponse User)
authUser (Credentials uName pass) = do
    con <- open "data/users.db"
    result <- query con "SELECT password FROM users WHERE name=?"
        (Only uName) :: IO [Only ByteString]
    verify con result
    where verify con [Only pw] =
             case verifyPass defaultParams
                 (Pass . pack $ pass) (EncryptedPass pw) of
               (True, _) -> do
                   key <- generateKey
                   execute con "UPDATE users SET api_key=?" (Only key) 
                   return . success $ User uName key
               _ -> return $ failure AuthError
          verify _ _ = return $ failure AuthError
