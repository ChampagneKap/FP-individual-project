{-# LANGUAGE OverloadedStrings #-}

module Database (
    initialiseDB,
    saveMessage,
    selectAllMessages,
    selectAllMessagesByUser
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
        execute_ conn "DELETE FROM messages"
        return conn

saveMessage :: Connection -> Message -> IO ()
saveMessage conn message = do
    let userFromID = userFrom message
    let userToID = userTo message
    let msg = msgContent message
    execute conn "INSERT INTO messages (content, userFrom, userTo) VALUES (?, ?, ?)" (msg, userFromID, userToID)

selectAllMessages :: Connection -> IO Int
selectAllMessages conn = do
    results <- query_ conn "SELECT * FROM messages" :: IO [Message]
    return (length results)

selectAllMessagesByUser :: Connection -> User -> IO ()
selectAllMessagesByUser conn user = do
    let receivingUser = userID user
    results <- query conn "SELECT * FROM messages WHERE userTo = ?" [receivingUser] :: IO [Message]
    let numOfMsgs = length results
    putStrLn ("User " ++ show receivingUser ++ " received " ++ show numOfMsgs ++ " messages.")