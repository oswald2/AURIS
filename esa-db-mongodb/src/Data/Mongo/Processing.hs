module Data.Mongo.Processing
    ( Data.Mongo.Processing.connect
    , withMongoDB
    ) where

import           RIO
import qualified RIO.Text                      as T
import           Prelude                        ( IOError )

import           Data.Attoparsec.Text          as A

import           UnliftIO.Exception            as E
import           Database.MongoDB.Connection   as DB

import           Data.Mongo.DbConfig


connect :: DbConfigMongoDB -> IO (Either Text Pipe)
connect cfg = do
    let h    = T.unpack (cfgMongoHost cfg)
        port = case cfgMongoPort cfg of
            Nothing -> Right defaultPort
            Just x  -> case parseOnly portParser x of
                Left err -> Left
                    ("Cannot parse MongoDB port identifier: " <> T.pack err)
                Right p -> Right p

    case port of
        Left  err -> return (Left err)
        Right p   -> do
            let conn = Host h p
            res <- E.try $ DB.connect conn
            case res of
                Left e ->
                    return
                        $ Left
                              (  "Error on connecting to MongoDB: "
                              <> T.pack (show (e :: IOError))
                              )
                Right c -> return $ Right c


portParser :: Parser PortID
portParser = do
    A.try (PortNumber <$> decimal) <|> (UnixSocket . T.unpack) <$> takeText


withMongoDB :: DbConfigMongoDB -> (Either Text Pipe -> IO (Either Text ())) -> IO (Either Text ())
withMongoDB cfg action = do
    bracket (Data.Mongo.Processing.connect cfg) shutdown action

  where
    shutdown (Left  err) = return (Left err)
    shutdown (Right _  ) = return (Right ())
