{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- | a user of our system
-- {{{
data User = User { userName :: String -- ^ name of the user
                 , apiKey :: String -- ^ user's API key
                 } deriving (Show, Read, Eq)

-- | Users are frequently passed to the API
instance ToJSON User where
    toJSON (User n a) = object ["name" .= n, "api_key" .= a]

-- | Users are returned from the API as well
instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "name" <*> v .: "api_key"
    parseJSON _ = mempty
-- }}}

-- | a group of users
-- {{{
newtype Group = Group { groupName :: String -- ^ the group's name
                      } deriving (Show, Read, Eq)

-- | Groups are only a newtype ATM, but that might change
instance ToJSON Group where
    toJSON (Group n) = object ["name" .= n]

-- | Groups are only a newtype ATM, but that might change
instance FromJSON Group where
    parseJSON (Object v) = Group <$> v .: "name"
    parseJSON _ = mempty
-- }}}

-- | a file
-- {{{
data File = File { name :: String -- ^ the file's name
                 , contents :: String -- ^ it's contents
                 } deriving (Show, Read, Eq)

-- | To encode files in JSON, we need to encode newlines on the fly 
instance ToJSON File where
    toJSON (File n c) = object ["name" .= n, "content" .= c']
        where c' = unlines $ splitOn "\n" c

-- | To decode files from JSON, we need to decode newlines on the fly
instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "name" <*>
        (intercalate "\n" . lines <$> v .: "content")
    parseJSON _ = mempty
-- }}}

-- | a list of files, with an owner
-- {{{
data FileList = FileList { ownerName :: String
                         , fileNames :: [String]
                         } deriving (Show, Read, Eq)

-- | FileLists are returned by an API call
instance ToJSON FileList where
    toJSON (FileList n fs) = object ["name" .= n, "files" .= fs]
-- }}}

-- | a request to push a file
-- {{{
data PushRequest = PushRequest User (Maybe Group) File

instance FromJSON PushRequest where
    parseJSON (Object v) = PushRequest <$>
        (v .: "user" >>= parseJSON) <*>
            (v .:? "group") <*> (v .: "file" >>= parseJSON)
    parseJSON _ = mempty
-- }}}

-- | A request to pull a file
-- {{{
data FileRequest = FileRequest User String 

instance FromJSON FileRequest where
    parseJSON (Object v) = FileRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "filename"
    parseJSON _ = mempty
-- }}}

-- | A request to create a group
-- {{{
data GroupRequest = GroupRequest User String

instance FromJSON GroupRequest where
    parseJSON (Object v) = GroupRequest <$>
        (v .: "user" >>= parseJSON) <*> v .: "groupname"
    parseJSON _ = mempty
-- }}}

-- | A request to add a fellow user to a group
-- {{{
data GroupAddRequest = GroupAddRequest User Group String

instance FromJSON GroupAddRequest where
    parseJSON (Object v) = GroupAddRequest <$>
        (v .: "user" >>= parseJSON) <*>
        (v .: "group" >>= parseJSON) <*>
        v .: "username"
    parseJSON _ = mempty
-- }}}

-- | A username and a password
-- {{{
data Credentials = Credentials String String

instance FromJSON Credentials where
    parseJSON (Object v) = Credentials <$> v .: "name" <*> v .: "password"
    parseJSON _ = mempty
-- }}}

-- | an error returned by the API
-- {{{
data ApiError = BadRequest
              | AuthError
              | EntityAlreadyExists
              | NoAccess
              | NoSuchEntity
    deriving (Show, Read, Eq)

-- | convert API errors to JSON
instance ToJSON ApiError where
    toJSON a = object ["name" .= show a]
-- }}}

-- | a response as returned by the API
-- {{{
newtype ApiResponse r = ApiResponse (Either ApiError r)
    deriving (Functor, Applicative, Monad)

-- | convert API responses to JSON
instance ToJSON r => ToJSON (ApiResponse r) where
    toJSON (ApiResponse (Left e)) =
        object ["result" .= Null, "error" .= e]
    toJSON (ApiResponse (Right r)) =
        object ["result" .= r, "error" .= Null]
-- }}}
