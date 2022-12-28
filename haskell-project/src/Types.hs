{-# LANGUAGE DeriveGeneric #-}

module Types (
    User (..),
    Message (..)
) where

data User = User {
    userID :: Int,
    firstName :: String,
    lastName :: String
} deriving (Show, Eq)

data Message = Message {
    msgContent :: String,
    userFrom :: User,
    userTo :: User
} deriving (Show, Eq)