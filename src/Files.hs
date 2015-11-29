{-# LANGUAGE OverloadedStrings #-}
module Files where

import Control.Monad (liftM)

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

import Database.SQLite.Simple

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath

import Group
import Types
import Util

-- | An owner can be used to check permissions on a String representing a file
class Owner o where
    queryF :: o -> Query
    queryL :: o -> Query
    queryU :: o -> Query
    queryI :: o -> Query
    getName :: o -> String

-- | Users are owners
instance Owner User where
    queryF = const "SELECT name FROM users INNER JOIN user_ownership ON \
               \user_ownership.path=? AND users.id=user_ownership.user_id \
               \ AND users.name=?"
    queryL = const "SELECT path FROM user_ownership INNER JOIN users ON \
               \user_ownership.user_id=users.id AND name=?"
    queryU = const "INSERT INTO user_ownership (user_id, path) VALUES (?, ?)"
    queryI = const "SELECT id FROM users WHERE name=?"
    getName = userName

-- | Groups are owners
instance Owner Group where
    queryF = const "SELECT name FROM groups INNER JOIN group_ownership ON \
               \group_ownership.path=? AND groups.id=group_ownership.group_id \
               \ AND groups.name=?"
    queryL = const "SELECT path FROM group_ownership INNER JOIN groups ON \
               \group_ownership.group_id=groups.id AND name=?"
    queryU = const "INSERT INTO group_ownership (group_id, path) VALUES (?, ?)"
    queryI = const "SELECT id FROM groups WHERE name=?"
    getName = groupName

-- | check permissions for a given owner
ownedBy :: Owner a => String -> a -> IO Bool
ownedBy file owner = do con <- open "data/users.db"
                        not . null <$> (query con (queryF owner)
                            (file, getName owner) :: IO [Only String])

-- | get the id of an owner
getId :: Owner a => a -> IO Int
getId owner = do con <- open "data/users.db"
                 [Only oId] <- query con (queryI owner) (Only $ getName owner)
                 return oId

-- | check whether a user is allowed to access a file
checkPermissions :: String -> User -> IO Bool
checkPermissions file user = do
    u <- ownedBy file user
    gs <- getGroups user
    (u ||) <$> (or <$> mapM (file `ownedBy`) gs)

-- | get a file as a User
getFile :: FileRequest -> IO (ApiResponse File)
getFile (FileRequest _ file) =
    liftM (success . File (cleanFilename file')) (readFile file')
    where file' = ("data/"++) . concat $ splitOn "../" file

-- | upload a file as a User or Group
uploadFile :: PushRequest -> IO (ApiResponse String)
uploadFile (PushRequest user Nothing file) = do
    createDirectoryIfMissing True (dropFileName filename)
    writeFile filename (contents file)
    con <- open "data/users.db"
    uId <- getId user
    execute con (queryU user) (uId, filename)
    return $ success (cleanFilename filename)
    where uName = userName user
          filename = sanitizeFilename file user
uploadFile (PushRequest _ (Just group) file) = do
    writeFile filename (contents file)
    con <- open "data/users.db"
    gId <- getId group
    execute con (queryU group) (gId, filename)
    return $ success (cleanFilename filename)
    where gName = groupName group
          filename = sanitizeFilename file group

-- | list all files available to a user
listFiles :: User -> IO (ApiResponse [FileList])
listFiles user@(User uName _) = do
    con <- open "data/users.db"
    groups <- getGroups user
    uFiles <- map (cleanFilename . fromOnly) <$> (query con (queryL user)
       (Only uName) :: IO [Only String])
    gFiles <- mapM (gHelper con) groups
    return . success $
        FileList uName uFiles :
        zipWith FileList (map groupName groups) gFiles
    where gHelper con gr = map (cleanFilename . fromOnly) <$>
             query con (queryL gr) (Only (getName gr))

-- | clean a filename from an optional "data/" prefix
cleanFilename :: FilePath -> FilePath
cleanFilename path
    | "data/" `isPrefixOf` path = drop 5 path
    | otherwise = path

-- | prevent directory transversal and add relative prefix
sanitizeFilename :: Owner o => File -> o -> FilePath
sanitizeFilename file owner = "data" </> getName owner </>
    foldr1 (</>) [dropSlash d | d <- splitOn ".." $ name file, not $ null d]
    where dropSlash ('/':s) = s
          dropSlash s = s

-- | does a file as requested by a user exist?
fileExisting :: Owner o => File -> o -> IO Bool
fileExisting file owner = doesFileExist $ sanitizeFilename file owner
