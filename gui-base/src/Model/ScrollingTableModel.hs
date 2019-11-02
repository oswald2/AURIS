{-|
Module      : GUI.ScrollingTableModel
Description : A model behind a table display
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides data types and functions for tables with rows. As new 
values are coming in, old rows are deleted and new ones are added on the top.
-}
{-# LANGUAGE 
    TemplateHaskell
#-}
module Model.ScrollingTableModel
  ( ToCellText(..)
  , ColumnDefinition(..)
  , DisplayCell(..)
  , TableModel
  , tableModelNew
  , queryTableModel 
  , queryTableModelUnlocked
  , tableModelSize
  , tableModelAddValue
  , tableModelLock
  , tableModelUnlock
  , columnNumber 
  , columnName
  , columnWidth
  , dispcText
  , dispcAlignment
  , mkColumnDefinitions
  )
where


import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.List                      as L
import           Control.Lens (makeLenses)

import qualified Data.Sequence                 as S

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations


-- | A columnm definition
data ColumnDefinition = ColumnDefinition {
    -- | The column number. Starts with 0 and specifies the order of the columns
    _columnNumber :: Int
    -- | The name of the column, displayed in the header 
    , _columnName :: Text
    -- | The width of the column in pixels
    ,_columnWidth :: Int
}
makeLenses ''ColumnDefinition

instance Eq ColumnDefinition where 
  c1 == c2 = _columnNumber c1 == _columnNumber c2

instance Ord ColumnDefinition where 
  compare c1 c2 = compare (_columnNumber c1) (_columnNumber c2)

-- | Creates a 'Vector' of 'ColumnDefinition', which are used internally
-- when displaying the values. The list is sorted based on the '_columnNumber'
-- from the 'ColumnDefinition' to get them in the right order.
-- This is just a convenience function
mkColumnDefinitions :: [ColumnDefinition] -> Vector ColumnDefinition
mkColumnDefinitions lst = V.fromList (L.sort lst)


-- | Specifies how the given 'Text' should be displayed.
data DisplayCell = DisplayCell {
    -- | The text to display 
    _dispcText :: !Text 
    -- | The 'Alignments' of the text in the cell of the table
    , _dispcAlignment :: !Alignments
}
makeLenses ''DisplayCell

-- | This class specifies for the @a how to render it in the table .
-- It is possible, that the request is out of the displayable parameters,
-- so a 'Maybe a' is received. On Nothing it should generally return an 
-- empty 'Text' in the 'DisplayCell'. 
--
-- Depending on the '_columnNumber' in the passed 'ColumnDefinition', it 
-- should display one of it's member parts in this column and therefore 
-- return a 'DisplayCell' with a 'Text' representation of the member part
-- for this column.
class ToCellText a where
    toCellText :: Maybe a -> ColumnDefinition -> DisplayCell


-- | A table model. Determines, how the data behind the table are stored.
-- Currently, this is basically a 'Seq' protected by an 'MVar' since we
-- are multithreaded
data TableModel a = TableModel {
  _tabMLock :: MVar ()
  , _tabMData :: IORef (Seq a)
  }

-- | Creates a new 'TableModel'
tableModelNew :: IO (TableModel a)
tableModelNew = TableModel <$> newMVar () <*> newIORef S.empty

-- | Locks the 'MVar' of the table model. This is necessary in the 
-- drawing functions of the table. The FLTKHS table calls the draw 
-- function for each cell, but the context of the cell is different.
-- It starts with StartPage, where the mutex is locked.
-- Then it loops over the cells where the data is taken out of the 
-- model and displayed. When reaching EndPage, the unlock is called.
tableModelLock :: TableModel a -> IO () 
tableModelLock model = takeMVar (_tabMLock model)


-- | Unlocks the 'MVar' again. This is generally done in the EndPage 
-- context of the table drawing functions
tableModelUnlock :: TableModel a -> IO () 
tableModelUnlock model = putMVar (_tabMLock model) ()


-- | A safe modification while the table is locked. The @action@ is 
-- called while the 'MVar' is locked and released when the action returns
withLockedTableModel :: TableModel a -> (TableModel a -> IO b) -> IO b
withLockedTableModel model action =
  bracket 
    (takeMVar (_tabMLock model))
    (putMVar (_tabMLock model)) 
    (\_ -> action model) 

-- | Lock the table and apply a querying function to the internal 
-- 'Seq' of the model. Returns the result
queryTableModel :: TableModel a -> (Seq a -> b) -> IO b
queryTableModel model f = do
  withLockedTableModel model 
    (\_ -> queryTableModelUnlocked model f)
  

-- | A unlocked version of the 'queryTableModel' function. Used inside 
-- the table drawing, when the 'MVar' is held in the StartPage context
-- and released in the EndPage context
queryTableModelUnlocked :: TableModel a -> (Seq a -> b) -> IO b 
queryTableModelUnlocked model f = f <$> readIORef (_tabMData model)


-- | returns the size of the model
tableModelSize :: TableModel a -> IO Int 
tableModelSize model = queryTableModel model S.length

-- | Adds a value to the model. It also takes a @modelMaxRows@ 
-- Int, which specifies how many rows are kept maximally. For
-- display purposes a few hundred is normally more than enough.
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

