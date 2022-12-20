{-# LANGUAGE DeriveGeneric #-}

module Types (
    User (..),
    Message (..)
) where

data User = User {
    userID :: Int,
    username :: String
} deriving (Show, Eq)

data Message = Message {
    msgID :: Int,
    userFrom :: String,
    userTo :: String
} deriving (Show, Eq)