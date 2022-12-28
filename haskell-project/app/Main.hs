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
    if (userFrom /= randomUser) then return randomUser else chooseRandomUser userFrom users

chooseRandomMsg :: IO String
chooseRandomMsg = do
    let msgs = ["hello", "cześć", "hola", "bonjour", "guten tag", "salve", "nǐn hǎo", "olá", "asalaam alaikum", "konnichiwa", "anyoung haseyo", "zdravstvuyte"]
    let len = (length msgs) - 1
    n <- randomRIO (0, len) :: IO Int
    let randomMsg = msgs!!n
    return randomMsg

-- createMessageToSend :: String -> User -> User -> Message
-- createMessageToSend msg userFrom userTo = Message msg userFrom userTo

-- sendMessage :: Connection -> MVar Message -> IO ()
-- sendMessage msgToSend = do
--     msg <- takeMVar msgToSend
--     if checkMessages then saveMessage conn msg
--     putMVar msgToSend msg

updateMsgList :: [Message] -> Message -> [Message]
updateMsgList [] msg = [msg]
updateMsgList (m:ms) msg = (m:ms) ++ [msg]

threadProcess :: Connection -> [User] -> User -> MVar [Message] -> IO ()
threadProcess conn users userFrom msgsSent = do
    generateRandomTimeInterval
    userTo <- chooseRandomUser userFrom users
    msgContent <- chooseRandomMsg
    let msg = Message msgContent userFrom userTo
    print msg
    listOfMsgs <- takeMVar msgsSent
    print listOfMsgs
    let listOfMsgs = updateMsgList listOfMsgs msg
    print listOfMsgs
    -- if (length listOfMsgs) < 100 then do
    --     listOfMsgs <- updateMsgList listOfMsgs msg
    --     print listOfMsgs
    -- else do 
    --     listOfMsgs <- listOfMsgs
    --     print listOfMsgs
    
    putMVar msgsSent listOfMsgs

spawnUserThreads :: Connection -> MVar [Message] -> [User] -> User -> IO ()
spawnUserThreads conn msgsSent users userFrom = do
    threadID <- forkIO (threadProcess conn users userFrom msgsSent)
    print threadID

main :: IO ()
main = do
    putStrLn "START"
    conn <- initialiseDB
    let users = map createUser [1..10]
    msgsSent <- newEmptyMVar
    mapM_ (spawnUserThreads conn msgsSent users) users
    putStrLn "FINISH"