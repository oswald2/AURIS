{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Data.MIB.Load
  ( loadFromFileGen
  )
where

import           RIO

import           RIO.Char
import           RIO.FilePath
import           RIO.Directory
import           RIO.ByteString.Lazy           as B
import           RIO.Text                      as T

import           Data.ByteString.Lazy.Char8    as BC
import           Data.Csv



myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }


loadFromFileGen
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack, FromRecord a)
  => FilePath
  -> FilePath
  -> m (Either Text (Vector a))
loadFromFileGen mibPath fileName = do
  let file = mibPath </> fileName
  ex <- doesFileExist file
  if ex
    then do
      logInfo $ "Reading file " <> display (T.pack fileName)
      content <- B.readFile file
      logInfo "File read. Parsing..."
      let !r = decodeWith myOptions NoHeader (BC.filter isAscii content)
      logInfo "Parsing Done."
      case r of
        Left  err -> pure $ Left (T.pack err)
        Right x   -> pure (Right x)
    else do
      return $! Left $ "File " <> T.pack file <> " does not exist."
