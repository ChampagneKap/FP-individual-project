module Main (main) where

import Types

import Control.Concurrent
import System.Random

createUser :: Int -> User
createUser n = User n ("User" ++ show n)

sendMsg :: String -> String -> IO ()
sendMsg user msg = do
    let userText = (user ++ " || ")
    putStrLn (userText ++ msg)

main :: IO ()
main = do
    let users = map createUser [1..10]
    let msgs = ["hello", "cześć", "hola", "bonjour", "guten tag", "salve", "nǐn hǎo", "olá", "asalaam alaikum", "konnichiwa", "anyoung haseyo", "zdravstvuyte"]
    let user1 = username (head users)
    let msg1 = head msgs
    threadID <- forkIO (sendMsg user1 msg1)
    putStrLn "FINISH"
