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

threadProcess :: Connection -> [User] -> MVar User -> IO ()
threadProcess conn users userFromBox = do
    userFrom <- takeMVar userFromBox
    print userFrom
    generateRandomTimeInterval
    userTo <- chooseRandomUser userFrom users
    print userTo
    msg <- chooseRandomMsg
    print msg
    saveMessage conn msg userFrom userTo
    putMVar userFromBox userFrom

main :: IO ()
main = do
    putStrLn "START"
    conn <- initialiseDB
    let users = map createUser [1..10]
    let user = head users
    userFromBox <- newMVar user
    forkIO (threadProcess conn users userFromBox)
    putStrLn "FINISH"