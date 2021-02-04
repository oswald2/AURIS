module Data.DbConfig.SQLite
    ( SQLiteConfig(..)
    ) where

import           RIO

import           Data.Aeson



data SQLiteConfig = SQLiteConfig 
  {
    sqConnString :: !Text
    , sqNumberConns :: !Word16
  }
    deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
