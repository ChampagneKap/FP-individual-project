{-# LANGUAGE OverloadedStrings #-}

module Database (
    initialiseDB,
    saveMessage
) where

import Types
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "messages.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS messages (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \content VARCHAR(50) NOT NULL, \
            \userFrom INTEGER DEFAULT NULL, \
            \userTo INTEGER DEFAULT NULL \
            \)"
        return conn

saveMessage :: Connection -> String -> User -> User -> IO ()
saveMessage conn msg userFrom userTo = do
    let userFromID = userID userFrom
    let userToID = userID userTo
    execute conn "INSERT INTO messages (content, userFrom, userTo) VALUES (?, ?, ?)" (msg, userFromID, userToID)