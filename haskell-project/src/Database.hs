{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database
Description : This module includes all the main functions necessary to manipulate the database
-}
module Database (
    -- * Functions
    initialiseDB,
    saveMessage,
    selectAllMessages,
    selectAllMessagesByUser,
    selectEachMessageSent
) where

import Types
import Database.SQLite.Simple

-- |The 'initialiseDB' function prepares a database in the `messages.sqlite` file to be used by the program
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "messages.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS messages (\
            \id INTEGER PRIMARY KEY,\
            \content VARCHAR(50) NOT NULL, \
            \userFrom INTEGER DEFAULT NULL, \
            \userTo INTEGER DEFAULT NULL \
            \)"
        -- The function also deletes all of the previous data from the table
        execute_ conn "DELETE FROM messages"
        return conn

-- |The 'saveMessage' function saves a given message into the database
saveMessage :: Connection -> Message -> IO ()
saveMessage conn message = do
    let userFromID = userFrom message
    let userToID = userTo message
    let msg = msgContent message
    execute conn "INSERT INTO messages (content, userFrom, userTo) VALUES (?, ?, ?)" (msg, userFromID, userToID)

-- |The 'selectAllMessages' function gets the count of all messages in the database
selectAllMessages :: Connection -> IO Int
selectAllMessages conn = do
    results <- query_ conn "SELECT * FROM messages" :: IO [Message]
    return (length results)

-- |The 'selectAllMessagesByUser' function gets the count of messages sent and recevied by a given user and displays it to the program user
selectAllMessagesByUser :: Connection -> User -> IO ()
selectAllMessagesByUser conn user = do
    let receivingUser = userID user
    sentResults <- query conn "SELECT * FROM messages WHERE userFrom = ?" [receivingUser] :: IO [Message]
    let numOfSentMsgs = length sentResults
    receivedResults <- query conn "SELECT * FROM messages WHERE userTo = ?" [receivingUser] :: IO [Message]
    let numOfReceivedMsgs = length receivedResults
    putStrLn ("User " ++ show receivingUser ++ " sent " ++ show numOfSentMsgs ++ " messages.")
    if userID user < 10 then putStrLn ("User " ++ show receivingUser ++ " received " ++ show numOfReceivedMsgs ++ " messages.\n")
    else putStrLn ("User " ++ show receivingUser ++ " received " ++ show numOfReceivedMsgs ++ " messages.")

-- |The 'selectEachMessageSent' function gets the count of each given message and displays it to the program user
selectEachMessageSent :: Connection -> String -> IO ()
selectEachMessageSent conn msg = do
    results <- query conn "SELECT * FROM messages WHERE content = ?" [msg] :: IO [Message]
    let numOfMsgs = length results
    putStrLn ("The message \"" ++  msg ++ "\" was sent " ++ show numOfMsgs ++ " times.")