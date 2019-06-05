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
    :: MonadIO m => ProtocolChannels a -> ConduitT (ProtocolPacket a) () m ()
protocolSwitcherC chans = do
    x <- await
    case x of
        Just pkt -> do
            case pkt ^. protInterface of
                IF_NCTRS -> do
                    atomically $ writeTBQueue (_prChNCTRS chans) (pkt ^. protContent)
                    protocolSwitcherC chans
                IF_CNC -> do
                    atomically $ writeTBQueue (_prChCnC chans) (pkt ^. protContent)
                    protocolSwitcherC chans
                IF_EDEN -> do
                    atomically $ writeTBQueue (_prChEDEN chans) (pkt ^. protContent)
                    protocolSwitcherC chans
                IF_EDEN_SCOE -> do
                    atomically $ writeTBQueue (_prChEDEN chans) (pkt ^. protContent)
                    protocolSwitcherC chans
        Nothing -> pure ()

