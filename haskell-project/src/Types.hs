{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Types
Description : This module includes all the type definitions required throughout the program
-}
module Types (
    -- * Types
    User (..),
    Message (..)
) where

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- | This is the data structure that will represent a User
data User = User {
    -- | The 'userID' method returns the id by which a user is indexed
    userID :: Int,
    -- | The 'firstName' method returns the first name of a user
    firstName :: String,
    -- | The 'lastName' method returns the last name of a user
    lastName :: String
} deriving (Show, Eq)

-- | This is the data structure that will represent a Message entry
data Message = Message {
    -- | The 'msgID' method returns the id of a message
    msgID :: Int,
    -- | The 'msgContent' method returns the content of a message
    msgContent :: String,
    -- | The 'userFrom' method returns the user a message was sent from
    userFrom :: Int,
    -- | The 'userTo' method returns the user a message was sent to
    userTo :: Int
} deriving (Show, Eq)

{-- Making Message an instance of FromRow and ToRow type classes --}

-- | Defining Message as an instance of FromRow
instance FromRow Message where
    fromRow = Message <$> field <*> field <*> field <*> field

-- | Defining Message as an instance of ToRow
instance ToRow Message where
    toRow (Message i cont uf ut) = toRow (i, cont, uf, ut)