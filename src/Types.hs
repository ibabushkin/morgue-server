{-# LANGUAGE TemplateHaskell
           , DeriveTraversable
           , DeriveFoldable
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           #-}

module Types
    ( module Types
    , module Export
    ) where

import Control.Applicative

import Crypto.Scrypt(Salt)

import Data.Aeson as Export
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet, ixFun, ixSet)
import Data.Morgue.AgendaGenerator as Export (AgendaMode(..))
import Data.Morgue.Format as Export (OutputFormat(..))
import qualified Data.Morgue.Options as O
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

-- = newtypes for names
-- | a User's name
newtype UserName = UserName { getUName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

-- | a Group's name
newtype GroupName = GroupName { getGName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

-- | a File's name
newtype FileName = FileName { getFName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

type FileContent = Text
type ApiKey = Text
type Password = ByteString

-- = To/FromJSON instances for elementar types and things exported by morgue
instance FromJSON ByteString where
    parseJSON (String s) = pure . pack $ T.unpack s
    parseJSON _ = mempty

-- | get setting data from a JSON string
instance FromJSON AgendaMode where
    parseJSON (String s) =
        case s of
          "Todo" -> pure Todo
          "Timed" -> pure Timed
          "Both" -> pure Both
          _ -> mempty
    parseJSON _ = mempty

-- | get setting data from a JSON string
instance FromJSON OutputFormat where
    parseJSON (String s) =
        case s of
          "ANSI" -> pure ANSI
          "Plaintext" -> pure Plaintext
          "Pango" -> pure Pango
          _ -> mempty
    parseJSON _ = mempty

-- | parse morgue's 'O.SimpleOptions' type
instance FromJSON O.SimpleOptions where
    parseJSON (Object v) = (O.SAgendaOptions <$>
        (v .: "mode") <*>
        (v .: "double_spaces") <*>
        (v .:? "tags") <*>
        (v .:? "skip_tags") <*>
        (v .: "num_days") <*>
        (v .: "format")) <|> (O.SOutlineOptions <$>
        (v .: "format"))
    parseJSON _ = mempty

-- = User definition
-- | a user of our system
data User = User { userName :: UserName -- ^ name of the user
                 , apiKey :: ApiKey -- ^ user's API key
                 } deriving (Show, Read, Eq, Ord)

-- | Users are frequently passed to the API
instance ToJSON User where
    toJSON (User n a) = object ["name" .= n, "api_key" .= a]

-- | Users are returned from the API as well
instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "name" <*> v .: "api_key"
    parseJSON _ = mempty

-- == Internal user representation
-- | internal representation of a user
data InternalUser = InternalUser
    { iUserName :: UserName
    , iApiKey :: ApiKey
    , iPassword :: Password
    , iUserFiles :: [File]
    } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- = User groups
-- | a group of users
data Group = Group { groupName :: GroupName
                   , users :: [UserName]
                   }

-- | Groups are returned from the API as well
instance ToJSON Group where
    toJSON (Group n u) = object ["name" .= n, "members" .= u]

-- == Internal group representation
-- | internal representation of a group
data InternalGroup = InternalGroup
    { iGroupName :: GroupName
    , iUsers :: [UserName]
    , iGroupFiles :: [File] -- maybe we should rather use a Set
    } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- = Files
-- | a file
data File = File
    { fileName :: FileName
    , fileContents :: FileContent
    } deriving (Show, Read, Data, Typeable)

-- | We only need to check for equality by name, since we just look whether
-- files with a certain name exist.
instance Eq File where
    a == b = fileName a == fileName b

-- | Ordering is name-oriented as well.
instance Ord File where
    a <= b = fileName a <= fileName b

instance ToJSON File where
    toJSON (File n c) = object ["name" .= n, "content" .= c]

instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "name" <*> v .: "content"
    parseJSON _ = mempty

-- == File lists
-- | a list of files belonging to a specific group
data GroupFileList = GroupFileList { gFileListName :: GroupName
                                   , gFileListFiles :: [FileName]
                                   }

instance FromJSON GroupFileList where
    parseJSON (Object v) = GroupFileList <$>
        (v .: "group") <*> (v .: "files")
    parseJSON _ = mempty

instance ToJSON GroupFileList where
    toJSON (GroupFileList gName gFiles) =
        object [ "group" .= gName, "files" .= gFiles]

-- | a list of files, possibly owned by up to one user and zero or more groups
data FileList = FileList
    { fUserFiles :: [FileName]
    , fGroupFiles :: [GroupFileList]
    }

instance FromJSON FileList where
    parseJSON (Object v) = FileList <$>
        (v .: "user_files") <*> (v .: "group_files")
    parseJSON _ = mempty

instance ToJSON FileList where
    toJSON (FileList uFiles gFiles) =
        object [ "user_files" .= uFiles
               , "group_files" .= gFiles
               ]

-- = Request types and intermediate data
-- == Pushing files
-- | a request to push a file to a user's filestore
data PushURequest = PushURequest { pURqUser :: User
                                 , pURqFile :: File
                                 }

instance FromJSON PushURequest where
    parseJSON (Object v) = PushURequest <$>
        (v .: "user" >>= parseJSON) <*> (v .: "file" >>= parseJSON)
    parseJSON _ = mempty

type PushUData = (Maybe InternalUser, File)

-- | a request to push a file to a group's filestore
data PushGRequest = PushGRequest { pGRqUser :: User
                                 , pGRqGroup :: GroupName
                                 , pGRqFile :: File
                                 }

instance FromJSON PushGRequest where
    parseJSON (Object v) = PushGRequest <$>
        (v .: "user" >>= parseJSON) <*> (v .: "group") <*>
            (v .: "file" >>= parseJSON)
    parseJSON _ = mempty

type PushGData = (Maybe InternalUser, Maybe InternalGroup, File)

-- == Listing files belonging to a user
-- | A request to list all available files
newtype ListRequest = ListRequest { lRqUser :: User }
    deriving FromJSON

type ListData = (Maybe InternalUser, [InternalGroup])

-- == Pulling files
-- | A request to pull a file from a user's filestore
data PullURequest = PullURequest { fURqUser :: User
                                 , fURqFileName :: FileName
                                 }

instance FromJSON PullURequest where
    parseJSON (Object v) = PullURequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "filename"
    parseJSON _ = mempty

type PullUData = (Maybe InternalUser, FileName)

-- | A request to pull a file from a group's filestore
data PullGRequest = PullGRequest { fGRqUser :: User
                                 , fGRqGroup :: GroupName
                                 , fGRqFileName :: FileName
                                 }

instance FromJSON PullGRequest where
    parseJSON (Object v) = PullGRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "group" <*> v .: "filename"
    parseJSON _ = mempty

type PullGData = (Maybe InternalUser, Maybe InternalGroup, FileName)

-- == Adding groups
-- | A request to create a group
data GroupNewRequest = GroupNewRequest { gRqUser :: User
                                       , gRqGroupName ::  GroupName
                                       }

instance FromJSON GroupNewRequest where
    parseJSON (Object v) = GroupNewRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "groupname"
    parseJSON _ = mempty

type GroupNewData = (Maybe InternalUser, GroupName, Maybe InternalGroup)

-- == Adding users to groups
-- | A request to add a fellow user to a group
data GroupAddRequest = GroupAddRequest { gaRqUser :: User
                                       , gaRqGroup ::  GroupName
                                       , gaRqUserName :: UserName
                                       }

instance FromJSON GroupAddRequest where
    parseJSON (Object v) = GroupAddRequest <$>
        (v .: "user" >>= parseJSON) <*>
        (v .: "group" >>= parseJSON) <*>
        v .: "username"
    parseJSON _ = mempty

type GroupAddData =
    (Maybe InternalUser, Maybe InternalGroup, Maybe InternalUser)

-- == Processing files to an agenda or outline
-- | A request to get an agenda or outline for a set of files
data ProcessingRequest = ProcessingRequest { prRqUser :: User
                                           , prRqOptions ::  O.SimpleOptions
                                           , prRqFiles :: FileList
                                           , utcTime :: UTCTime
                                           , timezone :: TimeZone
                                           }

type ProcessingData = (Maybe InternalUser, [InternalGroup], FileList,
    O.SimpleOptions, UTCTime, TimeZone)

data ProcessingRequest' = ProcessingRequest' { prRqUser' :: User
                                             , prRqOptions' ::  O.SimpleOptions
                                             , prRqFiles' :: FileList
                                             }

instance FromJSON ProcessingRequest' where
    parseJSON (Object v) = ProcessingRequest' <$>
        (v .: "user" >>= parseJSON) <*>
        (v .: "options" >>= parseJSON) <*>
        v .: "files"
    parseJSON _ = mempty

-- == Patching
-- | Request to patch a user's file
data PatchURequest = PatchURequest { paURqUser :: User
                                   , paURqFile :: FileName
                                   , paURqPatch :: Text
                                   }

type PatchUData = (Maybe InternalUser, FileName, Text)

instance FromJSON PatchURequest where
    parseJSON (Object v) = PatchURequest <$>
        (v .: "user" >>= parseJSON) <*>
        (v .: "file") <*>
        v .: "patch"

-- | Request to patch a group's file
data PatchGRequest = PatchGRequest { paGRqUser :: User
                                   , paGRqGroup :: GroupName
                                   , paGRqFile :: FileName
                                   , paGRqPatch :: Text
                                   }

type PatchGData = (Maybe InternalUser, Maybe InternalGroup, FileName, Text)

instance FromJSON PatchGRequest where
    parseJSON (Object v) = PatchGRequest <$>
        (v .: "user" >>= parseJSON) <*>
        (v .: "group") <*>
        (v .: "file") <*>
        v .: "patch"

-- == Authentication
-- | A username and a password
data Credentials = Credentials { cName :: UserName
                               , cPass :: Password
                               }

instance FromJSON Credentials where
    parseJSON (Object v) = Credentials <$> v .: "name" <*> v .: "password"
    parseJSON _ = mempty

-- | Request to sign up a new user, as used /internally/
data SignUpRequest = SignUpRequest { suRqCreds :: Credentials
                                   , suApiKey :: ApiKey
                                   , suSalt :: Salt
                                   }

-- | Request to sign up a new user, as passed to the API 
newtype SignUpRequest' = SignUpRequest' { suRqCreds' :: Credentials }
    deriving FromJSON

type SignUpData = (SignUpRequest, Maybe InternalUser)

-- | Request to authenticate as an existing user, as used /internally/
data SignInRequest = SignInRequest { siRqCreds :: Credentials
                                   , siApiKey :: ApiKey
                                   }

-- | Request to authenticate as an existing user, as pased to the API
newtype SignInRequest' = SignInRequest' { siRqCreds' :: Credentials }
    deriving FromJSON

type SignInData = (Password, Maybe InternalUser, ApiKey)


-- = API structure
-- | an error returned by the API
data ApiError = BadRequest
              | AuthError
              | NoAccess
              | NoSuchFile FileName
              | FileExists
              | NoSuchUser
              | UserExists
              | MemberExists
              | GroupExists
              | NoSuchGroup
    deriving (Show, Eq)

instance ToJSON ApiError where
    toJSON (NoSuchFile (FileName fName)) =
        String $ T.append "NoSuchFile: " fName
    toJSON a = String . T.pack $ show a

-- | a response as returned by the API
newtype ApiResponse r = ApiResponse (Either ApiError r)
    deriving (Functor, Applicative, Monad, Foldable, Traversable)

instance ToJSON r => ToJSON (ApiResponse r) where
    toJSON (ApiResponse (Left e)) =
        object ["result" .= Null, "error" .= e]
    toJSON (ApiResponse (Right r)) =
        object ["result" .= r, "error" .= Null]

-- = State datatypes used in the datastore
-- | our main application
data Morgue = Morgue { allUsers :: IxSet InternalUser
                     , allGroups :: IxSet InternalGroup
                     }

-- = 'SafeCopy' instances for all types in need
-- derive 'SafeCopy' instances for our types
$(deriveSafeCopy 0 'base ''O.SimpleOptions)
$(deriveSafeCopy 0 'base ''OutputFormat)
$(deriveSafeCopy 0 'base ''AgendaMode)
$(deriveSafeCopy 0 'base ''Salt)

$(deriveSafeCopy 0 'base ''Morgue)

$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 0 'base ''GroupName)
$(deriveSafeCopy 0 'base ''FileName)

$(deriveSafeCopy 0 'base ''Credentials)

$(deriveSafeCopy 0 'base ''InternalUser)
$(deriveSafeCopy 0 'base ''User)

$(deriveSafeCopy 0 'base ''InternalGroup)
$(deriveSafeCopy 0 'base ''Group)

$(deriveSafeCopy 0 'base ''File)
$(deriveSafeCopy 0 'base ''GroupFileList)
$(deriveSafeCopy 0 'base ''FileList)

$(deriveSafeCopy 0 'base ''SignUpRequest)
$(deriveSafeCopy 0 'base ''SignInRequest)
$(deriveSafeCopy 0 'base ''ListRequest)
$(deriveSafeCopy 0 'base ''PushURequest)
$(deriveSafeCopy 0 'base ''PushGRequest)
$(deriveSafeCopy 0 'base ''PullURequest)
$(deriveSafeCopy 0 'base ''PullGRequest)
$(deriveSafeCopy 0 'base ''GroupNewRequest)
$(deriveSafeCopy 0 'base ''GroupAddRequest)
$(deriveSafeCopy 0 'base ''ProcessingRequest)
$(deriveSafeCopy 0 'base ''PatchURequest)
$(deriveSafeCopy 0 'base ''PatchGRequest)

$(deriveSafeCopy 0 'base ''ApiError)
$(deriveSafeCopy 0 'base ''ApiResponse)

-- = 'Indexable' instances needed by the datastore
-- | we want to index a user by his name, API key and names of his files
instance Indexable InternalUser where
    empty = ixSet
        [ ixFun $ (:[]) . iUserName
        , ixFun $ \us -> [User (iUserName us) (iApiKey us)]
        , ixFun $ map fileName . iUserFiles
        ]

-- | we want to index a group by it's name, it's members' names, and names of
-- it's files
instance Indexable InternalGroup where
    empty = ixSet
        [ ixFun $ (:[]) . iGroupName
        , ixFun iUsers
        , ixFun $ map fileName . iGroupFiles
        ]
