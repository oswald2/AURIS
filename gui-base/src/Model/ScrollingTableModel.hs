{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
#-}
module Model.ScrollingTableModel
  ( ToCellText(..)
  , ColumnDefinition(..)
  , TableModel
  , tableModelNew
  , queryTableModel 
  , queryTableModelUnlocked
  , tableModelSize
  , tableModelAddValue
  , tableModelLock
  , tableModelUnlock
  )
where


import           RIO

import qualified Data.Sequence                 as S

import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations



data ColumnDefinition = ColumnDefinition {
    _columnName :: Text
    ,_columnWidth :: Int
}



class ToCellText a where
    toCellText :: Maybe a -> Column -> (Text, Alignments)


data TableModel a = TableModel {
  _tabMLock :: MVar ()
  , _tabMData :: IORef (Seq a)
  }

tableModelNew :: IO (TableModel a)
tableModelNew = TableModel <$> newMVar () <*> newIORef (S.empty)


tableModelLock :: TableModel a -> IO () 
tableModelLock model = takeMVar (_tabMLock model)

tableModelUnlock :: TableModel a -> IO () 
tableModelUnlock model = putMVar (_tabMLock model) ()



withLockedTableModel :: TableModel a -> (TableModel a -> IO b) -> IO b
withLockedTableModel model action =
  bracket 
    (takeMVar (_tabMLock model))
    (putMVar (_tabMLock model)) 
    (\_ -> action model) 


queryTableModel :: TableModel a -> (Seq a -> b) -> IO b
queryTableModel model f = do
  withLockedTableModel model 
    (\_ -> queryTableModelUnlocked model f)
  


queryTableModelUnlocked :: TableModel a -> (Seq a -> b) -> IO b 
queryTableModelUnlocked model f = f <$> readIORef (_tabMData model)

tableModelSize :: TableModel a -> IO Int 
tableModelSize model = queryTableModel model S.length


tableModelAddValue :: TableModel a -> Int -> a -> IO () 
tableModelAddValue model !modelMaxRows !x = do 
  withLockedTableModel model $ \_ -> do 
    dat <- readIORef (_tabMData model)

    let len = S.length dat 
        !newDat = if len < modelMaxRows
          then x S.<| dat
          else case dat of
            dropped S.:|> _x -> x S.<| dropped
            S.Empty          -> S.singleton x
    writeIORef (_tabMData model) newDat

