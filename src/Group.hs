{-# LANGUAGE OverloadedStrings #-}

module Group where

import Database.SQLite.Simple

import System.Directory (createDirectoryIfMissing)

import Types
import Util

existsGroup :: String -> IO Bool
existsGroup uName = do
    con <- open "data/users.db"
    (not . null) <$> (query con "SELECT name FROM groups WHERE name=?"
        (Only uName) :: IO [Only String])

-- | find out whether a user is a member of a group
isMember :: User -> Group -> IO Bool
isMember u g = (g`elem`) <$> getGroups u

-- | get a list of groups where a user is a member
getGroups :: User -> IO [Group]
getGroups (User uName _) = do
    con <- open "data/users.db"
    map (Group . fromOnly) <$> (query con "SELECT groups.name FROM \
        \groups, users INNER JOIN membership ON membership.user_id=users.id \
        \WHERE users.name=?" (Only uName) :: IO [Only String])

-- | get a list of users' names who are members of a group
getMembers :: GroupListRequest -> IO (ApiResponse [UserName])
getMembers (GroupListRequest _ (Group gName)) = do
    con <- open "data/users.db"
    (success .  map fromOnly) <$> (query con "SELECT users.name FROM \
        \groups, users INNER JOIN membership ON membership.user_id=users.id \
        \WHERE groups.name=?" (Only gName) :: IO [Only String])

-- | Take a name, check whether a group with that name exists already,
-- create it if possible
insertGroup :: GroupRequest -> IO (ApiResponse Group)
insertGroup (GroupRequest user gName) = do
    con <- open "data/users.db"
    execute con "INSERT INTO groups (name) VALUES (?)" (Only gName)
    createDirectoryIfMissing True ("data/" ++ gName)
    addUserToGroup (GroupAddRequest user (Group gName) (userName user))

-- | add a user to a group
addUserToGroup :: GroupAddRequest -> IO (ApiResponse Group)
addUserToGroup (GroupAddRequest _ (Group gName) uName) = do
    con <- open "data/users.db"
    [Only uId] <- query con "SELECT id FROM users WHERE name=?"
        (Only uName) :: IO [Only Int]
    [Only gId] <- query con "SELECT id FROM groups WHERE name=?"
        (Only gName) :: IO [Only Int]
    execute con "INSERT INTO membership (group_id, user_id) \
        \VALUES (?, ?)" (gId, uId)
    return . success $ Group gName
