module Main (main) where

import Database
import Types

import Control.Concurrent
import System.Random

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

main :: IO ()
main = do
    putStrLn "START"
    conn <- initialiseDB
    let users = map createUser [1..10]
    saveUsers conn users

    generateRandomTimeInterval
    let userFrom = head users
    userTo <- chooseRandomUser userFrom users
    msg <- chooseRandomMsg

    saveMessage conn msg userFrom userTo
    putStrLn "FINISH"
