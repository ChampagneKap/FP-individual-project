{-|
Module      : Main
Description : This module includes all the main functions of this app
-}
module Main (
    -- * Function
    main
) where

import Database
import Types
import Control.Concurrent
import System.Random
import Database.SQLite.Simple

-- |The 'createUser' function creates a new user
createUser :: Int -> User
createUser n = User n "User" (show n)

-- |The 'generateRandomTimeInterval' function chooses a random integer from the range and delays the thread by that amount
generateRandomTimeInterval :: IO ()
generateRandomTimeInterval = do
    n <- randomRIO (500, 5000) :: IO Int
    threadDelay n

-- |The 'chooseRandomUser' function chooses a random user from the list, checking it is not the same as the given user
chooseRandomUser :: User -> [User] -> IO User
chooseRandomUser userFrom users = do
    let len = length users - 1
    n <- randomRIO (0, len) :: IO Int
    let randomUser = users!!n
    if userFrom /= randomUser then return randomUser
    else chooseRandomUser userFrom users

-- |The 'chooseRandomMsg' function chooses a random message from the list
chooseRandomMsg :: IO String
chooseRandomMsg = do
    let msgs = ["hello", "cześć", "hola", "bonjour", "guten tag", "salve", "nǐn hǎo", "olá", "asalaam alaikum", "konnichiwa", "anyoung haseyo", "zdravstvuyte"]
    let len = length msgs - 1
    n <- randomRIO (0, len) :: IO Int
    let randomMsg = msgs!!n
    return randomMsg

-- |The 'sendMessage' function chooses a random message and receiving user and saves the message to the database
sendMessage :: Connection -> [User] -> User -> IO Message
sendMessage conn users userFrom = do
    userTo <- chooseRandomUser userFrom users
    msgContent <- chooseRandomMsg
    let msg = Message 0 msgContent (userID userFrom) (userID userTo)
    saveMessage conn msg
    return msg

-- |The 'threadProcess' function details the process for each thread to send a message
threadProcess :: Connection -> [User] -> User -> MVar Int -> MVar Message -> IO ()
threadProcess conn users userFrom msgsSent msgBox = do
    generateRandomTimeInterval
    -- The thread waits before attempting to take the MVar, to ensure only one thread sends a message at a time
    msgSentBefore <- takeMVar msgBox
    -- The thread queries the database to check if there have already been 100 messages sent
    numOfMsgs <- selectAllMessages conn
    if numOfMsgs < 100 then do
        msg <- sendMessage conn users userFrom
        putMVar msgBox msg
        threadProcess conn users userFrom msgsSent msgBox
    else do
        -- If 100 messages have already been sent, put a value into the check MVar to let the main thread know
        let updatedList = 100
        putMVar msgsSent updatedList

-- |The 'spawnUserThreads' function creates a new thread for each user
spawnUserThreads :: Connection -> MVar Int -> MVar Message -> [User] -> User -> IO ThreadId
spawnUserThreads conn msgsSent msgBox users userFrom = forkIO (threadProcess conn users userFrom msgsSent msgBox)

-- |The 'main' function creates the users, spawns the threads and display the stats to the user
main :: IO ()
main = do
    putStrLn "START - sending messages between users..."
    conn <- initialiseDB
    let users = map createUser [1..10]
    msgsSent <- newEmptyMVar
    msgBox <- newMVar (Message 0 "hi" 0 0)
    mapM_ (spawnUserThreads conn msgsSent msgBox users) users
    allMsgsSent <- takeMVar msgsSent
    putStrLn (show allMsgsSent ++ " messages sent between users.")
    putStrLn "FINISHED"
    putStrLn "=============================="
    mapM_ (selectAllMessagesByUser conn) users
    putStrLn "=============================="
    let msgs = ["hello", "cześć", "hola", "bonjour", "guten tag", "salve", "nǐn hǎo", "olá", "asalaam alaikum", "konnichiwa", "anyoung haseyo", "zdravstvuyte"]
    mapM_ (selectEachMessageSent conn) msgs
    putStrLn "=============================="