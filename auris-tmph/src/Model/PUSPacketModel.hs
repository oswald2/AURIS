{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
    , TemplateHaskell
#-}
module Model.PUSPacketModel
  ( PUSPacketModel
  , createPUSPacketModel
  , ModelValue(..)
  , ToCellText(..)
  , addPacketToModel
  , modelMaxRows
  , pusPacketModelSize
  , pusPktModelLock
  , pusPktModelData
  , pusPacketQueryUnlocked
)
where


import           RIO
import qualified RIO.Text                      as T

import           Control.Lens                   ( makeLenses )

import qualified Data.Sequence                 as S

import           Data.PUS.PUSPacket
import           Data.PUS.PUSDfh
import           Data.PUS.ExtractedDU

import           General.Hexdump

import           Model.ScrollingTableModel




newtype ModelValue = ModelValue { getModelValue :: ExtractedDU PUSPacket}

data PUSPacketModel = PUSPacketModel {
    _pusPktModelLock :: MVar ()
    , _pusPktModelData :: IORef (Seq ModelValue)
}
makeLenses ''PUSPacketModel


createPUSPacketModel :: IO PUSPacketModel
createPUSPacketModel = PUSPacketModel <$> newMVar () <*> newIORef (S.empty)


queryModel :: PUSPacketModel -> (Seq ModelValue -> a) -> IO a 
queryModel model f = do 
    takeMVar (model ^. pusPktModelLock)
    dat <- readIORef (model ^. pusPktModelData)

    let result = f dat 

    putMVar (model ^. pusPktModelLock) ()
    return result 

pusPacketQueryUnlocked :: PUSPacketModel -> (Seq ModelValue -> a) -> IO a 
pusPacketQueryUnlocked model f = do 
    dat <- readIORef (model ^. pusPktModelData) 
    pure (f dat)



addPacketToModel :: ExtractedDU PUSPacket -> PUSPacketModel -> IO PUSPacketModel
addPacketToModel pkt model = do
  takeMVar (model ^. pusPktModelLock)
  dat <- readIORef (model ^. pusPktModelData)

  let len    = S.length dat
      newDat = if len < modelMaxRows
        then dat S.|> ModelValue pkt
        else (S.drop 1 dat) S.|> ModelValue pkt

  writeIORef (model ^. pusPktModelData) newDat
  putMVar (model ^. pusPktModelLock) ()
  return model

pusPacketModelSize :: PUSPacketModel -> IO Int 
pusPacketModelSize model = queryModel model S.length


instance ToCellText ModelValue where
  toCellText Nothing _ = ""
  toCellText (Just (ModelValue pkt)) 0 =
    T.pack $ maybe "" show (pusPktTime (pkt ^. epDU . pusDfh))
  toCellText (Just _pkt) 1 = ""
  toCellText (Just (ModelValue pkt)) 2 =
    textDisplay (pkt ^. epDU . pusHdr . pusHdrTcApid)
  toCellText (Just (ModelValue pkt)) 3 =
    textDisplay (pusType (pkt ^. epDU . pusDfh))
  toCellText (Just (ModelValue pkt)) 4 =
    textDisplay (pusSubType (pkt ^. epDU . pusDfh))
  toCellText (Just (ModelValue pkt)) 5 =
    textDisplay (pkt ^. epDU . pusHdr . pusHdrTcSsc)
  toCellText (Just (ModelValue pkt)) 6 = hexdumpLineBS (pkt ^. epDU . pusData)
  toCellText (Just _               ) _ = ""
