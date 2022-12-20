{-# LANGUAGE OverloadedStrings #-}

module Database (
    initialiseDB,
    createUserInDB,
    saveUsers
) where

import Types
import Database.SQLite.Simple

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "messages.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS users (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \first_name VARCHAR(4), \
            \last_name VARCHAR(1) \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS messages (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \content VARCHAR(50) NOT NULL, \
            \userFrom INTEGER DEFAULT NULL, \
            \userTo INTEGER DEFAULT NULL \
            \)"
        return conn

createUserInDB :: Connection -> User -> IO ()
createUserInDB conn user = do
    let fn = firstName user
    let ln = lastName user
    execute conn "INSERT INTO users (first_name, last_name) VALUES (?, ?)" (fn, ln)

saveUsers :: Connection -> [User] -> IO ()
saveUsers conn = mapM_ (createUserInDB conn)