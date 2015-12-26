module Util where

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import Data.Char (isUpper)
import Data.Maybe (isJust, fromJust)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Happstack.Server (toResponse, Response)

import System.Directory
import System.FilePath ((</>), splitDirectories)

import Types

-- {{{ API helpers
-- | successful API response
success :: a -> ApiResponse i a
success = ApiResponse . Right

-- | failed API response
failure :: ApiError -> ApiResponse i a
failure = ApiResponse . Left

-- | process anything that can be encoded as JSON to a Response
respond :: ToJSON r => r -> Response
respond = toResponse . encodePretty' (defConfig { confCompare = compare })
-- }}}

-- {{{ datastore and path manipulation
-- | split a path to i
segmentPath :: FilePath -> FileName
segmentPath = FileName . splitDirectories

-- | remove dangerous chars from a String meant to be included in a path
cleanPathSegment :: String -> String
cleanPathSegment = filter (/='/')

-- | join a path in a secure fashion
joinPath :: FileName -> FilePath
joinPath = foldr ((</>) . cleanPathSegment) "" . getFName

-- | store an object
store :: ToJSON t => FileName -> t -> IO ()
store segments object =
    B.writeFile (joinPath segments) (encode object)

-- | check whether a file exists
exists :: FileName -> IO Bool
exists segments = doesFileExist (joinPath segments)

-- | load an object
load :: FromJSON t => FileName -> IO (Maybe t)
load segments = do
    ex <- exists segments
    if ex
       then decode <$> B.readFile (joinPath segments)
       else return Nothing

loadAll :: FromJSON t => FilePath -> IO [t]
loadAll path = do
    paths <- getDirectoryContents path
    objects <- mapM load [segmentPath p | p <- paths, ".json" `isSuffixOf` p]
    return . map fromJust $ filter isJust objects

-- | get a file's content as text if it is present
loadText :: FileName -> IO (Maybe T.Text)
loadText segments = do
    ex <- exists segments
    if ex
       then Just <$> T.IO.readFile (joinPath segments)
       else return Nothing
-- }}}

-- {{{ ApiRequest combinators
-- | wrap a simple IO action that returns a result to be usable as `finish`
wrapFinish :: (a -> IO b) -> ApiResponse r a -> IO (ApiResponse r b)
wrapFinish f a = sequence $ f <$> a

-- | wrap a IO-check, an error and a getter to be usable as `verify`
wrapVerify :: (r -> IO Bool) -> ApiError -> (r -> IO i) -> r
           -> IO (ApiResponse r i)
wrapVerify check err get req = do
    res <- check req
    if res
       then success <$> get req
       else return $ failure err
-- }}}

-- {{{ ApiResponse wrappers
fromBoolIO :: (a -> IO Bool) -> ApiError -> a -> IO (ApiResponse r a)
fromBoolIO check err arg = do
    res <- check arg
    if res
       then return $ success arg
       else return $ failure err

fromBool :: (a -> Bool) -> ApiError -> a -> ApiResponse r a
fromBool pred err a
    | pred a = success a
    | otherwise = failure err
-- }}}
