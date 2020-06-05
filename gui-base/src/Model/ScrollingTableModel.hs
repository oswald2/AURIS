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
  , TableModel(..)
  , VectorTableModel
  , MVectorTableModel
  -- , tableModelNew
  -- , tableModelNewFromFoldable
  -- , queryTableModel
  -- , queryTableModelUnlocked
  -- , tableModelSize
  -- , tableModelAddValue
  -- , tableModelSetValues
  -- , tableModelLock
  -- , tableModelUnlock
  , columnNumber
  , columnName
  , columnWidth
  , dispcText
  , dispcAlignment
  , dispcCellColor
  , dispcTextColor
  , defDisplayCell
  , displayCell
  , mkColumnDefinitions
  )
where


import           RIO
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Unsafe             as V 
import qualified Data.Vector.Mutable as VM
import qualified RIO.List                      as L
import           Control.Lens (makeLenses)
import           Data.Kind 

import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           GUI.Colors



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
    -- | The color in which the background of the cell should be drawn.
    -- | Can be used to "highlight" cells
    , _dispcCellColor :: !Color
    -- | Text color of the content of the cell
    , _dispcTextColor :: !Color
}
makeLenses ''DisplayCell

-- | Default values for a table cell
defDisplayCell :: DisplayCell
defDisplayCell = DisplayCell "" alignLeft mcsTableBG mcsTableFG

-- | Get a display cell with default values, but text and alignment set. This
-- is a convenience function.
displayCell :: Text -> Alignments -> DisplayCell
displayCell txt align = defDisplayCell { _dispcText = txt, _dispcAlignment = align}

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
    toCellText :: a -> ColumnDefinition -> DisplayCell



class TableModel (tm :: Type -> Type)  where 
  -- | Creates a new 'TableModel'
  tableModelNew :: IO (tm a)
  -- | Creates a new 'TableModel' and fills it from the 'Foldable'
  tableModelNewFromFoldable :: Foldable t => t a -> IO (tm a)
  -- | Locks the 'MVar' of the table model. This is necessary in the
  -- drawing functions of the table. The FLTKHS table calls the draw
  -- function for each cell, but the context of the cell is different.
  -- It starts with StartPage, where the mutex is locked.
  -- Then it loops over the cells where the data is taken out of the
  -- model and displayed. When reaching EndPage, the unlock is called.
  tableModelLock :: tm a -> IO ()
  -- | Unlocks the 'MVar' again. This is generally done in the EndPage
  -- context of the table drawing functions
  tableModelUnlock :: tm a -> IO ()
  -- | Performs the operation on the 'TableModel' with a locked mutex and then 
  -- automatically unlocks it again
  withLockedTableModel :: tm a -> (tm a -> IO b) -> IO b
  withLockedTableModel model action =
    bracket
      (tableModelLock model) 
      (const (tableModelUnlock model))
      (\_ -> action model)

  -- | Lock the table and apply a querying function to the internal
  -- 'Vector' of the model. Returns the result
  queryTableModel :: tm a -> (Vector a -> b) -> IO b
  queryTableModel model f = do
    withLockedTableModel model 
      (\_ -> queryTableModelUnlocked model f)

  -- | A unlocked version of the 'queryTableModel' function. Used inside
  -- the table drawing, when the 'MVar' is held in the StartPage context
  -- and released in the EndPage context
  queryTableModelUnlocked :: tm a -> (Vector a -> b) -> IO b

  -- | Get the value at the index x unlocked
  tableModelIndexUnlocked :: tm a -> Int -> IO (Maybe a)

  -- | returns the size of the model
  tableModelSize :: tm a -> IO Int
  tableModelSizeUnlocked :: tm a -> IO Int 

  -- | Adds a value to the model. It also takes a @modelMaxRows@
  -- Int, which specifies how many rows are kept maximally. For
  -- display purposes a few hundred is normally more than enough.
  tableModelAddValue :: tm a -> Int -> a -> IO ()

  -- | Set the complete model values at once from a 'Foldable'. Old values
  -- are discarded
  tableModelSetValues :: (Foldable t) => tm a -> t a -> IO ()







-- | A table model. Determines, how the data behind the table are stored.
-- Currently, this is basically a 'Vector' protected by an 'MVar' since we
-- are multithreaded
data VectorTableModel a = VectorTableModel {
  _tabMLock :: MVar ()
  , _tabMData :: IORef (Vector a)
  }

instance TableModel VectorTableModel where 
  tableModelNew = VectorTableModel <$> newMVar () <*> newIORef V.empty
  
  tableModelNewFromFoldable t = do
    let v = V.fromList (toList t)
    VectorTableModel <$> newMVar () <*> newIORef v
  
  tableModelLock model = takeMVar (_tabMLock model)
  tableModelUnlock model = putMVar (_tabMLock model) ()

  queryTableModelUnlocked model f = f <$> readIORef (_tabMData model)

  tableModelIndexUnlocked model idx = (V.!? idx) <$> readIORef (_tabMData model)

  tableModelSize model = queryTableModel model V.length
  tableModelSizeUnlocked model = queryTableModelUnlocked model V.length

  tableModelAddValue model !modelMaxRows !x = do
    withLockedTableModel model $ \_ -> do
      dat <- readIORef (_tabMData model)

      let len = V.length dat
          !newDat 
            | V.null dat = V.singleton x 
            | len < modelMaxRows = V.cons x dat
            | otherwise = x `V.cons` V.slice 0 (len - 1) dat 
      writeIORef (_tabMData model) newDat

  tableModelSetValues model t = do
    let !v = V.fromList (toList t)
    withLockedTableModel model $ \_ -> do
      writeIORef (_tabMData model) v



-- | A table model. Determines, how the data behind the table are stored.
-- Currently, this is basically a 'MVector' protected by an 'MVar' since we
-- are multithreaded
data MVectorTableModel a = MVectorTableModel {
  _tabMVLock :: MVar ()
  , _tabMVData :: IORef (VM.IOVector a)
  }


instance TableModel MVectorTableModel where 

  tableModelNew = MVectorTableModel <$> newMVar () <*> (VM.new 0 >>= newIORef)

  tableModelNewFromFoldable t = do
    let v = V.fromList (toList t)
    MVectorTableModel <$> newMVar () <*> (V.thaw v >>= newIORef)

  tableModelLock model = takeMVar (_tabMVLock model)
  tableModelUnlock model = putMVar (_tabMVLock model) ()

  queryTableModelUnlocked model f = do 
    v <- readIORef (_tabMVData model)
    f <$> V.freeze v

  tableModelIndexUnlocked model idx = do 
    v <- readIORef (_tabMVData model) 
    if idx < VM.length v 
      then Just <$> VM.read v idx
      else return Nothing


  tableModelSize model = queryTableModel model V.length
  tableModelSizeUnlocked model = queryTableModelUnlocked model V.length

  tableModelAddValue model !modelMaxRows !x = do
    withLockedTableModel model $ \_ -> do
      dat <- readIORef (_tabMVData model)

      let len = VM.length dat
          newDat 
            | VM.null dat = do 
                v <- VM.new 1 
                VM.unsafeWrite v 0 x 
                return v 
            | len < modelMaxRows = do
                v' <- VM.grow dat 1
                prepend v' x 
                return v'
            | otherwise = do 
                prepend dat x 
                return dat 
      v' <- newDat 
      writeIORef (_tabMVData model) v'
    where 
      prepend vec val = do 
        let len = VM.length vec
            indexes = [len - 1, len - 2 .. 1]
            
            upd i = do 
              val' <- VM.unsafeRead vec (i-1)
              VM.unsafeWrite vec i val'
        mapM_ upd indexes 
        VM.unsafeWrite vec 0 val

  tableModelSetValues model t = 
    withLockedTableModel model $ \m -> do 
      let len = length t 
          cont = zip [0..] (toList t)
      vec <- VM.new len 
      mapM_ (uncurry (VM.unsafeWrite vec)) cont 
      writeIORef (_tabMVData m) vec   
      
