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



modelMaxRows :: Int
modelMaxRows = 200


data ColumnDefinition = ColumnDefinition {
    _columnName :: Text,
    _columnWidth :: Int
} 



class ToCellText a where
    toCellText :: Maybe a -> Int -> Text


