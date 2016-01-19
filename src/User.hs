{-# LANGUAGE OverloadedStrings #-}
module User where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Crypto.Scrypt

import Data.Acid (Query, Update)
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.IxSet hiding (delete)
import Data.List (delete)
import qualified Data.Morgue.Agenda as A
import qualified Data.Morgue.Outline as O
import Data.Morgue.Options
import Data.Text (pack, unpack)
import Data.Time

import System.Entropy

import Group (toGroupFileList)
import Types
import Util

-- = Utility functions

-- TODO: maybe incorporate user data (or use JWT and STFU)
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

-- | transform a SignInRequest as passed to our application to a version
-- suited for pure computation
toSignInRequest :: SignInRequest' -> IO SignInRequest
toSignInRequest req =
    SignInRequest (siRqCreds' req) <$> genApiKey

-- | transform a ProcessingRequest as passed to our application to a version
-- suited for pure computation
toProcessingRequest :: ProcessingRequest' -> IO ProcessingRequest
toProcessingRequest req =
    ProcessingRequest (prRqUser' req) (prRqOptions' req) (prRqFiles' req) <$>
        getCurrentTime <*> getCurrentTimeZone

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

-- = Signing up
-- | provide data needed for signup. We don't do any IO here
signUpProvider :: SignUpRequest -> Query Morgue SignUpData
signUpProvider req = do
    morgue <- ask
    return (req, getOne $ allUsers morgue @= cName (suRqCreds req))

-- | process data from a sign up request
makeUser :: SignUpData -> ApiResponse InternalUser
makeUser (SignUpRequest creds key salt, Nothing) = success $
    InternalUser (cName creds) key encPass []
    where encPass = B.fromStrict . getEncryptedPass $
              encryptPass' salt (Pass . B.toStrict $ cPass creds)
makeUser (_, Just _) = failure UserExists

-- = Logging in
-- | provide data from a login request 
signInProvider :: SignInRequest -> Query Morgue SignInData
signInProvider (SignInRequest creds key) = do
    morgue <- ask
    return ( cPass creds
           , getOne $ allUsers morgue @= cName creds
           , key
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

-- = Uploading a file
-- | provide data from a user-push request
pushUProvider :: PushURequest -> Query Morgue PushUData
pushUProvider (PushURequest user file) = do
    morgue <- ask
    return (getOne $ allUsers morgue @= user, file)

-- | add a file to a user's files
addFileToUser :: PushUData -> ApiResponse InternalUser
addFileToUser (Just user, file)
    | file `elem` iUserFiles user = failure FileExists
    | otherwise = success $ user { iUserFiles = file : iUserFiles user }
addFileToUser (Nothing, _) = failure AuthError

-- | get a user's last file 
getLastFile :: InternalUser -> FileName
getLastFile = fileName . head . iUserFiles

-- = Deleting a file
-- | provide data from a user-delete request
deleteUProvider :: DeleteURequest -> Query Morgue DeleteUData
deleteUProvider (DeleteURequest user fName) = do
    morgue <- ask
    return (getOne $ allUsers morgue @= user, fName)

-- | delete a file from a user's datastore
deleteFileFromUser :: DeleteUData -> ApiResponse InternalUser
deleteFileFromUser (Nothing, _) = failure AuthError
deleteFilefromUser (Just user, fName)
    | oldLen == length newFiles = failure $ NoSuchFile fName
    | otherwise = success $ user { iUserFiles = newFiles }
    where newFiles = delete (File fName "") (iUserFiles user)
          oldLen = length $ iUserFiles user

-- = Pulling a file
-- | provide data from a user-pull request
pullUProvider :: PullURequest -> Query Morgue PullUData
pullUProvider (PullURequest user fName) = do
    morgue <- ask
    return (getOne $ allUsers morgue @= user, fName)

-- | get a file from a user, looking it up by name
getFileFromUser :: PullUData -> ApiResponse File
getFileFromUser (Just user, fName) =
    case filter ((==fName) . fileName) $ iUserFiles user of
      [file] -> success file
      _ -> failure $ NoSuchFile fName
getFileFromUser (Nothing, _) = failure AuthError

-- = Listing available files
-- | provide data from a user-list request
listProvider :: ListRequest -> Query Morgue ListData
listProvider req = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , toList $ allGroups morgue @= userName user
           )
    where user = lRqUser req

-- | list all files from a user and a list of groups
toFileList :: ListData -> ApiResponse FileList
toFileList (Just user, groups) = success $
    FileList (map fileName $ iUserFiles user)
        (map toGroupFileList groups)
toFileList (Nothing, _) = failure AuthError

-- = processing files to an agenda
-- | provide data from a processing request
processingProvider :: ProcessingRequest -> Query Morgue ProcessingData
processingProvider (ProcessingRequest user opts fList tz time) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , toList $ allGroups morgue @= user
           , fList
           , opts
           , tz
           , time
           )

-- | process files
processFiles :: ProcessingData -> ApiResponse String
processFiles (Just user, groups, FileList uFiles gFiles, opts, time, tz) = 
    processor <$> files
    where opts' = convertOptions opts
          files = (unpack . mconcat . mconcat) <$>
              ((:) <$> matchFiles (iUserFiles user) uFiles <*>
               (matchGroups groups gFiles >>= mapM (uncurry matchFiles)))
          processor = case opts of
                        SAgendaOptions{} -> A.getAgenda opts' tz time
                        SOutlineOptions{} -> O.getOutline opts'
processFiles (Nothing, _, _, _, _, _) = failure AuthError

-- = patching files
-- | provide data from a patching request
patchUProvider :: PatchURequest -> Query Morgue PatchUData
patchUProvider (PatchURequest user fName patch) = do
    morgue <- ask
    return ( getOne $ allUsers morgue @= user
           , fName
           , patch
           )

-- | process a patch in an impure fashion ;(
processUPatch :: PatchUData -> IO (ApiResponse (InternalUser, File))
processUPatch (Nothing, _, _) = return $ failure AuthError
processUPatch (Just user@(InternalUser _ _ _ uFiles), fName, patch) =
    case matchFiles uFiles [fName] of
      ApiResponse (Right [content]) -> do
          newFile <- patchFile (File fName content) patch
          return $ success
              (user { iUserFiles = replaceFile uFiles newFile }, newFile)
      ApiResponse (Right _) -> return . failure $ NoSuchFile fName
      ApiResponse (Left err) -> return $ failure err
