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
    , prChNCTRS
    , prChCnC
    , prChEDEN
    , protocolSwitcherC
    , createProtocolChannels
    )
where

import           RIO

import           Conduit
import           Data.Conduit.TQueue

import           Control.Lens                   ( makeLenses )

import           Protocol.ProtocolInterfaces



data ProtocolChannels a = ProtocolChannels {
   _prChNCTRS :: TBQueue (ProtocolPacket a)
   , _prChCnC :: TBQueue (ProtocolPacket a)
   , _prChEDEN :: TBQueue (ProtocolPacket a)
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
    :: MonadIO m => ProtocolChannels a -> ConduitT (ProtocolPacket a) () m ()
protocolSwitcherC chans = do
    x <- await
    case x of
        Just pkt -> do
            case pkt ^. protInterface of
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
        Nothing -> pure ()

