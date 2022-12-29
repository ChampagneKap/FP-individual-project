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
    --print n
    threadDelay n

chooseRandomUser :: User -> [User] -> IO User
chooseRandomUser userFrom users = do
    let len = (length users) - 1
    n <- randomRIO (0, len) :: IO Int
    let randomUser = users!!n
    if (userFrom /= randomUser) then return randomUser
    else chooseRandomUser userFrom users

chooseRandomMsg :: IO String
chooseRandomMsg = do
    let msgs = ["hello", "cześć", "hola", "bonjour", "guten tag", "salve", "nǐn hǎo", "olá", "asalaam alaikum", "konnichiwa", "anyoung haseyo", "zdravstvuyte"]
    let len = (length msgs) - 1
    n <- randomRIO (0, len) :: IO Int
    let randomMsg = msgs!!n
    return randomMsg

updateMsgList :: [Message] -> Message -> [Message]
updateMsgList [] msg = [msg]
updateMsgList (m:ms) msg = (m:ms) ++ [msg]

sendMessage :: Connection -> [User] -> User -> IO Message
sendMessage conn users userFrom = do
    generateRandomTimeInterval
    userTo <- chooseRandomUser userFrom users
    msgContent <- chooseRandomMsg
    let msg = Message msgContent userFrom userTo
    saveMessage conn msg
    return msg

threadProcess :: Connection -> [User] -> User -> MVar [Message] -> [Message] -> IO ()
threadProcess conn users userFrom msgsBox msgsSent = do
    if (length msgsSent) < 100 then do
        msg <- sendMessage conn users userFrom
        let updatedList = updateMsgList msgsSent msg
        threadProcess conn users userFrom msgsBox updatedList
    else do
        let updatedList = msgsSent
        putMVar msgsBox updatedList

spawnUserThreads :: Connection -> MVar [Message] -> [User] -> User -> IO ThreadId
spawnUserThreads conn msgsSent users userFrom = forkIO (threadProcess conn users userFrom msgsSent [])

main :: IO ()
main = do
    putStrLn "START - sending messages between users..."
    conn <- initialiseDB
    let users = map createUser [1..10]
    msgsSent <- newEmptyMVar
    mapM_ (spawnUserThreads conn msgsSent users) users
    allMsgsSent <- takeMVar msgsSent
    print (length allMsgsSent)
    putStrLn "FINISH"