module Main (main) where

import Database
import Types

createUser :: Int -> User
createUser n = User n "User" (show n)

main :: IO ()
main = do
    conn <- initialiseDB
    let users = map createUser [1..10]
    saveUsers conn users
    let userFrom = head users
    let userTo = users!!5
    let msg = "hello there"
    saveMessage conn msg userFrom userTo
    putStrLn "FINISH"
