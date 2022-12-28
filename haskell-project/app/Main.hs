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
    n <- randomRIO (500000, 2000000) :: IO Int
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

threadProcess :: Connection -> [User] -> User -> MVar [Message] -> IO ()
threadProcess conn users userFrom msgsSent = do
    generateRandomTimeInterval
    userTo <- chooseRandomUser userFrom users
    msgContent <- chooseRandomMsg
    let msg = Message msgContent userFrom userTo
    saveMessage conn msg

    listOfMsgs <- tryTakeMVar msgsSent
    case listOfMsgs of
        Nothing -> do
            let updatedList = updateMsgList [] msg
            putMVar msgsSent updatedList
            threadProcess conn users userFrom msgsSent
        Just msgs -> do
            if (length msgs) < 100 then do
                let updatedList = updateMsgList msgs msg
                putMVar msgsSent updatedList
                threadProcess conn users userFrom msgsSent
            else do
                let updatedList = msgs
                putMVar msgsSent updatedList

spawnUserThreads :: Connection -> MVar [Message] -> [User] -> User -> IO ThreadId
spawnUserThreads conn msgsSent users userFrom = forkIO (threadProcess conn users userFrom msgsSent)

main :: IO ()
main = do
    putStrLn "START"
    conn <- initialiseDB
    let users = map createUser [1..10]
    msgsSent <- newEmptyMVar
    mapM_ (spawnUserThreads conn msgsSent users) users
    allMsgsSent <- takeMVar msgsSent
    print allMsgsSent
    putStrLn "FINISH"