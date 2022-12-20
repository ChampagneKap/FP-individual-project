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
    putStrLn "FINISH"
