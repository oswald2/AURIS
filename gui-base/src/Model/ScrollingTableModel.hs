{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
#-}
module Model.ScrollingTableModel
  ( ToCellText(..)
  , modelMaxRows
  , ColumnDefinition(..)
  )
where


import           RIO
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations



modelMaxRows :: Int
modelMaxRows = 200


data ColumnDefinition = ColumnDefinition {
    _columnName :: Text,
    _columnWidth :: Int
} 



class ToCellText a where
    toCellText :: Maybe a -> Column -> (Text, Alignments)


