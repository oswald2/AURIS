{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
    , TemplateHaskell
#-}
module Protocol.Switcher
    ( ProtocolChannels
    , NCTRSChan
    , prChNCTRS
    , prChCnC
    , prChEDEN
    , protocolSwitcherC
    , createProtocolChannels

    )
where

import           RIO

import           Conduit

import           Control.Lens                   ( makeLenses )

import           Protocol.ProtocolInterfaces



type NCTRSChan a = TBQueue a

data ProtocolChannels a = ProtocolChannels {
   _prChNCTRS :: NCTRSChan a
   , _prChCnC :: TBQueue a
   , _prChEDEN :: TBQueue a
   }
makeLenses ''ProtocolChannels

queueSize :: Natural
queueSize = 500


createProtocolChannels :: MonadIO m => m (ProtocolChannels a)
createProtocolChannels = do
    atomically $ do
        ProtocolChannels
            <$> newTBQueue queueSize
            <*> newTBQueue queueSize
            <*> newTBQueue queueSize


protocolSwitcherC
    :: (MonadIO m, ProtocolDestination a) => ProtocolChannels a -> ConduitT a Void m ()
protocolSwitcherC chans = awaitForever $ \pkt ->
    case destination pkt of
        IF_NCTRS -> do
            atomically $ writeTBQueue (_prChNCTRS chans) pkt
            protocolSwitcherC chans
        IF_CNC -> do
            atomically $ writeTBQueue (_prChCnC chans) pkt
            protocolSwitcherC chans
        IF_EDEN -> do
            atomically $ writeTBQueue (_prChEDEN chans) pkt
            protocolSwitcherC chans
        IF_EDEN_SCOE -> do
            atomically $ writeTBQueue (_prChEDEN chans) pkt
            protocolSwitcherC chans

