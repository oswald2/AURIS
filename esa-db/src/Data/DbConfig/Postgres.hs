module Data.DbConfig.Postgres
    ( PostgresConfig(..)
    , defaultPostgresConfig
    , getConnString
    ) where

import           RIO
import qualified Data.ByteString.Char8         as B
import           Database.Persist.Postgresql
import           ByteString.StrictBuilder

import           Data.Aeson


data PostgresConfig = PostgresConfig
    { pgHost        :: !Text
    , pgPort        :: !Word16
    , pgUser        :: !Text
    , pgPassword    :: !Text
    , pgDbName      :: !Text
    , pgNumberConns :: !Word16
    }
    deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

defaultPostgresConfig :: PostgresConfig
defaultPostgresConfig = PostgresConfig { pgHost        = "localhost"
                                       , pgPort        = 5432
                                       , pgUser        = "auris"
                                       , pgPassword    = "auris"
                                       , pgDbName      = "active_session"
                                       , pgNumberConns = 10
                                       }


getConnString :: PostgresConfig -> ConnectionString
getConnString PostgresConfig {..} =
    builderBytes
        $  bytes "host="
        <> bytes (encodeUtf8 pgHost)
        <> bytes " port="
        <> bytes (B.pack (show pgPort))
        <> bytes " user="
        <> bytes (encodeUtf8 pgUser)
        <> bytes " password="
        <> bytes (encodeUtf8 pgPassword)
        <> bytes " dbname="
        <> bytes (encodeUtf8 pgDbName)




--  "host=localhost port=5432 user=test dbname=test password=test"
