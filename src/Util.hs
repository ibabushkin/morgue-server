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

-- | successful API response
success :: a -> ApiResponse a
success = ApiResponse . Right

-- | failed API response
failure :: ApiError -> ApiResponse a
failure = ApiResponse . Left

-- | process anything that can be encoded as JSON to a Response
respond :: ToJSON r => r -> Response
respond = toResponse . encodePretty' (defConfig { confCompare = compare })

-- | split a path to i
segmentPath :: FilePath -> FileName
segmentPath = splitDirectories

-- | remove dangerous chars from a String meant to be included in a path
cleanPathSegment :: String -> String
cleanPathSegment = filter (/='/')

-- | join a path in a secure fashion
joinPath :: FileName -> FilePath
joinPath = foldr (</>) "" . map cleanPathSegment

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
