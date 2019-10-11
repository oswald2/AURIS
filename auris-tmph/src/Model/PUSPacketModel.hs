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

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations




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
        then ModelValue pkt S.<| dat
        else case dat of
          dropped S.:|> _x -> ModelValue pkt S.<| dropped
          S.Empty          -> S.singleton (ModelValue pkt)

  writeIORef (model ^. pusPktModelData) newDat
  putMVar (model ^. pusPktModelLock) ()
  return model

pusPacketModelSize :: PUSPacketModel -> IO Int
pusPacketModelSize model = queryModel model S.length


instance ToCellText ModelValue where
  toCellText Nothing _ = ("", alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 0) =
    (T.pack $ maybe "" show (pusPktTime (pkt ^. epDU . pusDfh)), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 1) = (textDisplay (pkt ^. epERT), alignLeft)
  toCellText (Just (ModelValue pkt)) (Column 2) =
    (textDisplay (pkt ^. epDU . pusHdr . pusHdrTcApid), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 3) =
    (textDisplay (pusType (pkt ^. epDU . pusDfh)), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 4) =
    (textDisplay (pusSubType (pkt ^. epDU . pusDfh)), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 5) =
    (textDisplay (pkt ^. epDU . pusHdr . pusHdrTcSsc), alignRight)
  toCellText (Just (ModelValue pkt)) (Column 6) =
    (hexdumpLineBS (pkt ^. epDU . pusData), alignLeft)
  toCellText (Just _) _ = ("", alignLeft)
