module Main (main) where

import Database
import Types

import Control.Concurrent
import System.Random
import Database.SQLite.Simple

createUser :: Int -> User
createUser n = User n "User" (show n)

generateRandomTimeInterval :: IO ()
generateRandomTimeInterval = do
    n <- randomRIO (50, 200) :: IO Int
    threadDelay n

chooseRandomUser :: User -> [User] -> IO User
chooseRandomUser userFrom users = do
    let len = length users - 1
    n <- randomRIO (0, len) :: IO Int
    let randomUser = users!!n
    if userFrom /= randomUser then return randomUser
    else chooseRandomUser userFrom users

chooseRandomMsg :: IO String
chooseRandomMsg = do
    let msgs = ["hello", "cześć", "hola", "bonjour", "guten tag", "salve", "nǐn hǎo", "olá", "asalaam alaikum", "konnichiwa", "anyoung haseyo", "zdravstvuyte"]
    let len = length msgs - 1
    n <- randomRIO (0, len) :: IO Int
    let randomMsg = msgs!!n
    return randomMsg

updateMsgList :: [Message] -> Message -> [Message]
updateMsgList [] msg = [msg]
updateMsgList (m:ms) msg = (m:ms) ++ [msg]

sendMessage :: Connection -> [User] -> User -> IO Message
sendMessage conn users userFrom = do
    userTo <- chooseRandomUser userFrom users
    msgContent <- chooseRandomMsg
    let msg = Message 0 msgContent (userID userFrom) (userID userTo)
    saveMessage conn msg
    return msg

threadProcess :: Connection -> [User] -> User -> MVar Int -> MVar Message -> IO ()
threadProcess conn users userFrom msgsSent msgBox = do
    generateRandomTimeInterval
    numOfMsgs <- selectAllMessages conn
    if numOfMsgs < 100 then do
        msgSentBefore <- takeMVar msgBox
        msg <- sendMessage conn users userFrom
        putMVar msgBox msg
        threadProcess conn users userFrom msgsSent msgBox
    else do
        let updatedList = 100
        putMVar msgsSent updatedList

spawnUserThreads :: Connection -> MVar Int -> MVar Message -> [User] -> User -> IO ThreadId
spawnUserThreads conn msgsSent msgBox users userFrom = forkIO (threadProcess conn users userFrom msgsSent msgBox)

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
    putStrLn "FINISH"
    mapM_ (selectAllMessagesByUser conn) users