{-# LANGUAGE DeriveGeneric #-}

module Types (
    User (..),
    Message (..)
) where

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

data User = User {
    userID :: Int,
    firstName :: String,
    lastName :: String
} deriving (Show, Eq)

data Message = Message {
    msgID :: Int,
    msgContent :: String,
    userFrom :: Int,
    userTo :: Int
} deriving (Show, Eq)

instance FromRow Message where
    fromRow = Message <$> field <*> field <*> field <*> field

instance ToRow Message where
    toRow (Message i cont uf ut) = toRow (i, cont, uf, ut)