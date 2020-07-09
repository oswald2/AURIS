{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.TMFrame
  ( TMFrame (..)
  , databaseSink
  )
  where

import           RIO
import           Database.Selda
import           Persistence.Internals ( Table'(..), databaseSink )


-- FIXME: Here we use incorrect types because selda does not support Words yet.
data TMFrame  = TMFrame
  { ert   :: Int -- Word64 -- ^ Earth reception time
  , scid  :: Int -- Word16 -- ^ Spacecraft ID
  , vcid  :: Int -- Word16 -- ^ Virtual channel ID
  , mcfc  :: Int -- Word8  -- ^ Master channel frame count
  , vcfc  :: Int -- Word8  -- ^ Virtual channel frame count
  , frame :: ByteString -- ^ Raw frame data
  }
  deriving (Generic, Show)

instance SqlRow TMFrame

instance Table' TMFrame where
  table' = table "tm_frames"
    [ #ert  :- indexUsing BTreeIndex
    , #scid :- index
    , #vcid :- index
    ]
