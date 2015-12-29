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
import Data.List.Split (splitOn)
import Data.Morgue.AgendaGenerator as Export (AgendaMode(..))
import Data.Morgue.Format as Export (OutputFormat(..))
import qualified Data.Morgue.Options as O
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Text (Text)

import Text.Read (readMaybe)

-- {{{ type synonyms for self-documenting types
newtype UserName = UserName { getUName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

newtype GroupName = GroupName { getGName :: Text }
    deriving (Show, Read, Eq, Ord, Data, FromJSON, ToJSON)

-- | a File's name, by path components
newtype FileName = FileName { getFName :: [Text] }
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

instance FromJSON O.Options where
    parseJSON (Object v) = (O.AgendaOptions <$>
        (v .: "mode") <*>
        (v .: "double_spaces") <*>
        (v .:? "tags") <*>
        (v .:? "skip_tags") <*>
        (v .: "num_days") <*>
        pure Prelude.putStrLn <*>
        (v .: "format")) <|> (O.OutlineOptions <$>
        pure Prelude.putStrLn <*>
        (v .: "format"))
        where helper (Just a) = return a
              helper Nothing = fail ""
-- }}}

-- | a user of our system
-- {{{
data User = User { userName :: UserName -- ^ name of the user
                 , apiKey :: ApiKey -- ^ user's API key
                 } deriving (Show, Read, Eq)

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
-- }}}

-- | internal representation of a group
-- {{{
data InternalGroup = InternalGroup
    { iGroupName :: GroupName
    , iUsers :: [UserName]
    , iGroupFiles :: [File]
    } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- }}}

-- | a file
-- {{{
data File = File
    { fileName :: FileName
    , fileContents :: FileContent
    } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | To encode files in JSON, we need to encode newlines on the fly 
instance ToJSON File where
    toJSON (File n c) = object ["name" .= n, "content" .= c]

-- | To decode files from JSON, we need to decode newlines on the fly
instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "name" <*> v .: "content"
    parseJSON _ = mempty
-- }}}

-- | a list of files, with an owner
-- {{{
data FileList = FileList { ownerName :: UserName
                         , fileNames :: [FileName]
                         } deriving (Show, Read, Eq)

-- | FileLists are returned by an API call
instance ToJSON FileList where
    toJSON (FileList n fs) = object ["name" .= n, "files" .= fs]

type FileListData = (UserName, [GroupName])
-- }}}

-- | a request to push a file
-- {{{
data PushRequest = PushRequest { pRqUser :: User
                               , pRqGroup ::  Maybe GroupName
                               , pRqFile ::  File
                               }

instance FromJSON PushRequest where
    parseJSON (Object v) = PushRequest <$>
        (v .: "user" >>= parseJSON) <*>
            (v .:? "group") <*> (v .: "file" >>= parseJSON)
    parseJSON _ = mempty

type PushData = (FileList, File)
-- }}}

-- | A request to pull a file
-- {{{
data FileRequest = FileRequest { fRqUser :: User
                               , fRqFileName :: FileName
                               }

instance FromJSON FileRequest where
    parseJSON (Object v) = FileRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "filename"
    parseJSON _ = mempty
-- }}}

-- | A request to create a group
-- {{{
data GroupRequest = GroupRequest { gRqUser :: User
                                 , gRqGroupName ::  GroupName
                                 }

instance FromJSON GroupRequest where
    parseJSON (Object v) = GroupRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "groupname"
    parseJSON _ = mempty
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

type GroupAddData = (Group, UserName)
-- }}}

-- | A request to get an agenda or outline for a set of files
-- {{{
data ProcessingRequest = ProcessingRequest { prRqUser :: User
                                           , prRqOptions ::  O.Options
                                           , prRqFileNames :: [FileName]
                                           }

instance FromJSON ProcessingRequest where
    parseJSON (Object v) = ProcessingRequest <$>
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
newtype ApiResponse i r = ApiResponse (Either ApiError r)
    deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | convert API responses to JSON
instance ToJSON r => ToJSON (ApiResponse i r) where
    toJSON (ApiResponse (Left e)) =
        object ["result" .= Null, "error" .= e]
    toJSON (ApiResponse (Right r)) =
        object ["result" .= r, "error" .= Null]
-- }}}

-- | an API call
type ApiCall req res = req -> Update Morgue (ApiResponse req res) 

-- | our main application
data Morgue = Morgue { allUsers :: IxSet InternalUser
                     , allGroups :: IxSet InternalGroup
                     }

-- {{{ derive SafeCopy instances for our types
$(deriveSafeCopy 0 'base ''Salt)

$(deriveSafeCopy 0 'base ''Morgue)

$(deriveSafeCopy 0 'base ''UserName)

$(deriveSafeCopy 0 'base ''GroupName)

$(deriveSafeCopy 0 'base ''FileName)

$(deriveSafeCopy 0 'base ''InternalUser)

$(deriveSafeCopy 0 'base ''User)

$(deriveSafeCopy 0 'base ''InternalGroup)

$(deriveSafeCopy 0 'base ''File)

$(deriveSafeCopy 0 'base ''Credentials)

$(deriveSafeCopy 0 'base ''SignUpRequest)

$(deriveSafeCopy 0 'base ''SignInRequest)

$(deriveSafeCopy 0 'base ''ApiError)

$(deriveSafeCopy 0 'base ''ApiResponse)
-- }}}

-- | we want to index a user by his name, API key and names of his files
instance Indexable InternalUser where
    empty = ixSet
        [ ixFun $ (:[]) . iUserName
        , ixFun $ (:[]) . iApiKey
        , ixFun $ map fileName . iUserFiles
        ]

-- | we want to index a group by it's name, it's members' names, and names of
-- it's files
instance Indexable InternalGroup where
    empty = ixSet
        [ ixFun $ (:[]) . iGroupName
        , ixFun $ iUsers
        , ixFun $ map fileName . iGroupFiles
        ]
