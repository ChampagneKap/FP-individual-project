module Main (main) where

import Types

import Control.Concurrent
import System.Random

createUser :: Int -> User
createUser n = User n username
    where username = "User " ++ show n

main :: IO ()
main = do
    let users = map createUser [1..10]
    print (head users)
