module Util where

import Data.Acid (Update)
import Data.Aeson.Encode.Pretty
import Data.IxSet (empty)
import Data.Text (pack, unpack, Text)
import Data.Text.IO (hPutStr)
import qualified Data.Text.IO as TIO

import Happstack.Server (toResponse, Response)

import System.Directory (removeFile)
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import System.Process

import Types

-- = API helpers
-- | successful API response
success :: a -> ApiResponse a
success = ApiResponse . Right

-- | failed API response
failure :: ApiError -> ApiResponse a
failure = ApiResponse . Left

-- | process anything that can be encoded as JSON to a Response
respond :: ToJSON r => r -> Response
respond = toResponse . encodePretty' (defConfig { confCompare = compare })

-- | lift a storeage action \"onto\" the 'ApiResponse' monad
liftStore :: (a -> Update Morgue b)
          -> ApiResponse a -> Update Morgue (ApiResponse b)
liftStore f a = sequence $ f <$> a

-- | initial state of our app
initialMorgueState :: Morgue
initialMorgueState = Morgue empty empty

-- | patch a file using the common UNIX utility
patchFile :: File -> Text -> IO File
patchFile file patch = do
    (path, handle) <- openTempFile (unpack . getFName $ fileName file) ""
    hPutStr handle (fileContents file)
    hClose handle
    output <- readProcess "patch" [path] (unpack patch)
    newFile <- TIO.readFile path
    removeFile path
    return file { fileContents = newFile }

-- | get all matching files from a list and return an error in case of
-- missing files
matchFiles :: [File] -> [FileName] -> ApiResponse [FileContent]
matchFiles files = foldr go (success [])
    where files' = map ((,) <$> fileName <*> id) files
          go f fs = case lookup f files' of
                      Just file -> (fileContents file:) <$> fs
                      Nothing -> failure $ NoSuchFile f

-- | get all matching groups from a list and return an error in case of
-- missing groups
matchGroups :: [InternalGroup]
            -> [GroupFileList] -> ApiResponse [([File], [FileName])]
matchGroups groups = foldr go (success [])
    where groups' = map ((,) <$> iGroupName <*> id) groups
          go (GroupFileList g fs) gs = case lookup g groups' of
                                         Just gr ->
                                             ((iGroupFiles gr, fs):) <$> gs
                                         Nothing -> failure NoAccess

-- | replace a file by name
replaceFile :: [File] -> File -> [File]
replaceFile files newFile = foldr go [] files
    where go f@(File fName _) fs
              | fName == fileName newFile = newFile : fs
              | otherwise = f : fs
