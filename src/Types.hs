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
    , module O
    ) where

import Control.Applicative

import Crypto.Scrypt(Salt)

import Data.Acid (Update)
import Data.Aeson as Export
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Data (Data, Typeable)
import Data.IxSet ( Indexable(..), IxSet(..), (@=)
                  , Proxy(..), getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.List (intercalate)
import Data.Morgue.AgendaGenerator as Export (AgendaMode(..))
import Data.Morgue.Format as Export (OutputFormat(..))
import qualified Data.Morgue.Options as O
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

import Text.Read (readMaybe)

-- {{{ type synonyms for self-documenting types
newtype UserName = UserName { getUName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

newtype GroupName = GroupName { getGName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

-- | a File's name, by path components
newtype FileName = FileName { getFName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

type FileContent = Text
type ApiKey = Text
type Password = ByteString
-- }}}

-- {{{ To/FromJSON instances for elementar types
instance FromJSON ByteString where
    parseJSON (String s) = pure . pack $ T.unpack s
    parseJSON _ = mempty

instance ToJSON ByteString where
    toJSON = String . T.pack . unpack
-- }}}

-- {{{ FromJSON instances for morgue's datatypes
instance FromJSON AgendaMode where
    parseJSON (String s) =
        case s of
          "Todo" -> pure Todo
          "Timed" -> pure Timed
          "Both" -> pure Both
          _ -> mempty
    parseJSON _ = mempty

instance FromJSON OutputFormat where
    parseJSON (String s) =
        case s of
          "ANSI" -> pure ANSI
          "Plaintext" -> pure Plaintext
          "Pango" -> pure Pango
          _ -> mempty
    parseJSON _ = mempty

instance FromJSON O.SimpleOptions where
    parseJSON (Object v) = (O.SAgendaOptions <$>
        (v .: "mode") <*>
        (v .: "double_spaces") <*>
        (v .:? "tags") <*>
        (v .:? "skip_tags") <*>
        (v .: "num_days") <*>
        (v .: "format")) <|> (O.SOutlineOptions <$>
        (v .: "format"))
        where helper (Just a) = return a
              helper Nothing = fail ""
-- }}}

-- | a user of our system
-- {{{
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
-- }}}

-- | internal representation of a user
-- {{{
data InternalUser = InternalUser
    { iUserName :: UserName
    , iApiKey :: ApiKey
    , iPassword :: Password
    , iUserFiles :: [File]
    } deriving (Eq, Ord, Show, Read, Data, Typeable)
-- }}}

-- | a group of users
-- {{{
data Group = Group { groupName :: GroupName
                   , users :: [UserName]
                   }

-- | Groups are returned from the API as well
instance ToJSON Group where
    toJSON (Group n u) = object ["name" .= n, "members" .= u]

-- }}}

-- | internal representation of a group
-- {{{
data InternalGroup = InternalGroup
    { iGroupName :: GroupName
    , iUsers :: [UserName]
    , iGroupFiles :: [File] -- maybe we should rather use a Set
    } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- }}}

-- | a file
-- {{{
data File = File
    { fileName :: FileName
    , fileContents :: FileContent
    } deriving (Show, Read, Data, Typeable)

instance Eq File where
    a == b = fileName a == fileName b

instance Ord File where
    a <= b = fileName a <= fileName b

-- | To encode files in JSON, we need to encode newlines on the fly 
instance ToJSON File where
    toJSON (File n c) = object ["name" .= n, "content" .= c]

-- | To decode files from JSON, we need to decode newlines on the fly
instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "name" <*> v .: "content"
    parseJSON _ = mempty
-- }}}

-- | a list of files, possibly owned by up to one user and zero or more groups
-- {{{
data GroupFileList = GroupFileList { gFileListName :: GroupName
                                   , gFileListFiles :: [FileName]
                                   }

instance FromJSON GroupFileList where
    parseJSON (Object v) = GroupFileList <$>
        (v .: "group") <*> (v .: "files")

instance ToJSON GroupFileList where
    toJSON (GroupFileList gName gFiles) =
        object [ "group" .= gName, "files" .= gFiles]

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

-- }}}

-- | a request to push a file
-- {{{
data PushURequest = PushURequest { pURqUser :: User
                                 , pURqFile :: File
                                 }

instance FromJSON PushURequest where
    parseJSON (Object v) = PushURequest <$>
        (v .: "user" >>= parseJSON) <*> (v .: "file" >>= parseJSON)
    parseJSON _ = mempty

type PushUData = (Maybe InternalUser, File)

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
-- }}}

-- | A request to list all available files
-- {{{
newtype ListRequest = ListRequest { lRqUser :: User }
    deriving FromJSON

type ListData = (Maybe InternalUser, [InternalGroup])
-- }}}

-- | A request to pull a file
-- {{{
data PullURequest = PullURequest { fURqUser :: User
                                 , fURqFileName :: FileName
                                 }

instance FromJSON PullURequest where
    parseJSON (Object v) = PullURequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "filename"
    parseJSON _ = mempty

type PullUData = (Maybe InternalUser, FileName)

data PullGRequest = PullGRequest { fGRqUser :: User
                                 , fGRqGroup :: GroupName
                                 , fGRqFileName :: FileName
                                 }

instance FromJSON PullGRequest where
    parseJSON (Object v) = PullGRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "group" <*> v .: "filename"
    parseJSON _ = mempty

type PullGData = (Maybe InternalUser, Maybe InternalGroup, FileName)

-- }}}

-- | A request to create a group
-- {{{
data GroupNewRequest = GroupNewRequest { gRqUser :: User
                                       , gRqGroupName ::  GroupName
                                       }

instance FromJSON GroupNewRequest where
    parseJSON (Object v) = GroupNewRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "groupname"
    parseJSON _ = mempty

type GroupNewData = (Maybe InternalUser, GroupName, Maybe InternalGroup)
-- }}}

-- | A request to add a fellow user to a group
-- {{{
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

-- }}}

-- | A request to get an agenda or outline for a set of files
-- {{{
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
-- }}}

-- | A username and a password
-- {{{
data Credentials = Credentials { uName :: UserName
                               , uPass :: Password
                               }

instance FromJSON Credentials where
    parseJSON (Object v) = Credentials <$> v .: "name" <*> v .: "password"
    parseJSON _ = mempty
-- }}}

-- | types for signing in and up
-- {{{
data SignUpRequest = SignUpRequest { suRqCreds :: Credentials
                                   , suApiKey :: ApiKey
                                   , suSalt :: Salt
                                   }

newtype SignUpRequest' = SignUpRequest' { suRqCreds' :: Credentials }
    deriving FromJSON

type SignUpData = (SignUpRequest, Maybe InternalUser)

data SignInRequest = SignInRequest { siRqCreds :: Credentials
                                   , siApiKey :: ApiKey
                                   }

newtype SignInRequest' = SignInRequest' { siRqCreds' :: Credentials }
    deriving FromJSON

type SignInData = (Password, Maybe InternalUser, ApiKey)

-- }}}

-- | an error returned by the API
-- {{{
data ApiError = BadRequest
              | AuthError
              | IllegalName
              | NoAccess
              | NoSuchEntity
              | EntityAlreadyExists
    deriving (Show, Eq)

instance ToJSON ApiError where
    toJSON = String . T.pack . show

-- | a response as returned by the API
-- {{{
newtype ApiResponse r = ApiResponse (Either ApiError r)
    deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | convert API responses to JSON
instance ToJSON r => ToJSON (ApiResponse r) where
    toJSON (ApiResponse (Left e)) =
        object ["result" .= Null, "error" .= e]
    toJSON (ApiResponse (Right r)) =
        object ["result" .= r, "error" .= Null]
-- }}}

-- | our main application
data Morgue = Morgue { allUsers :: IxSet InternalUser
                     , allGroups :: IxSet InternalGroup
                     }

-- {{{ derive SafeCopy instances for our types
$(deriveSafeCopy 0 'base ''O.SimpleOptions)
$(deriveSafeCopy 0 'base ''OutputFormat)
$(deriveSafeCopy 0 'base ''AgendaMode)

$(deriveSafeCopy 0 'base ''Salt)

$(deriveSafeCopy 0 'base ''Morgue)

$(deriveSafeCopy 0 'base ''UserName)

$(deriveSafeCopy 0 'base ''GroupName)

$(deriveSafeCopy 0 'base ''FileName)

$(deriveSafeCopy 0 'base ''InternalUser)

$(deriveSafeCopy 0 'base ''User)

$(deriveSafeCopy 0 'base ''InternalGroup)

$(deriveSafeCopy 0 'base ''Group)

$(deriveSafeCopy 0 'base ''File)

$(deriveSafeCopy 0 'base ''GroupFileList)

$(deriveSafeCopy 0 'base ''FileList)

$(deriveSafeCopy 0 'base ''Credentials)

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

$(deriveSafeCopy 0 'base ''ApiError)

$(deriveSafeCopy 0 'base ''ApiResponse)
-- }}}

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
