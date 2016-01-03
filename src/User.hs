module User where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Crypto.Scrypt

import Data.Acid (Query, Update)
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.IxSet
import Data.Text (pack)

import System.Entropy

import Group (toGroupFileList)
import Types
import Util

-- {{{ utility functions

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

-- | store a user, updating it if told so
packUser :: Bool -> InternalUser -> Update Morgue InternalUser
packUser update user = do
    morgue <- get
    put $ morgue { allUsers = func user (allUsers morgue) }
    return user
    where func | update = updateIx (iUserName user)
               | otherwise = insert

-- | store a new user
storeUser :: InternalUser -> Update Morgue InternalUser
storeUser = packUser False

-- | update an existing user
updateUser :: InternalUser -> Update Morgue InternalUser
updateUser = packUser True

-- | conversion for public access
toUser :: InternalUser -> User
toUser = User <$> iUserName <*> iApiKey
-- }}}

-- {{{ signing up

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
-- }}}

-- {{{ logging in

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
-- }}}

-- {{{ uploading a file

-- | provide data from a user-push request
pushUProvider :: PushURequest -> Query Morgue PushUData
pushUProvider (PushURequest user file) = do
    morgue <- ask
    return (getOne $ allUsers morgue @= user, file)

-- | add a file to a user's files
addFileToUser :: PushUData -> ApiResponse InternalUser
addFileToUser (Just user, file)
    | file `elem` (iUserFiles user) = failure EntityAlreadyExists
    | otherwise = success $ user { iUserFiles = file : iUserFiles user }
addFileToUser (Nothing, _) = failure AuthError

-- | get a user's last file 
getLastFile :: InternalUser -> FileName
getLastFile = fileName . head . iUserFiles

-- }}}

-- {{{ downloading a file

-- | provide data from a user-pull request
pullUProvider :: PullURequest -> Query Morgue PullUData
pullUProvider (PullURequest user fileName) = do
    morgue <- ask
    return (getOne $ allUsers morgue @= user, fileName)

-- | get a file from a user, looking it up by name
getFileFromUser :: PullUData -> ApiResponse File
getFileFromUser (Just user, fName) =
    case filter ((==fName) . fileName) $ iUserFiles user of
      [file] -> success file
      [] -> failure NoSuchEntity
getFileFromUser (Nothing, _) = failure AuthError

-- }}}

-- {{{ listing available files

-- | provide data from a user-list request
listProvider :: ListRequest -> Query Morgue ListData
listProvider req = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , toList $ allGroups morgue @= (userName user)
           )
    where user = lRqUser req

-- | list all files from a user and a list of groups
toFileList :: ListData -> ApiResponse FileList
toFileList (Just user, groups) = success $
    FileList (iUserName user) (map fileName $ iUserFiles user)
        (map toGroupFileList groups)
toFileList (Nothing, _) = failure AuthError
-- }}}
