module GUI.TMFrameTable
  ( colDefinitions
  )
where

import           RIO

import           Model.ScrollingTableModel




colDefinitions :: Vector ColumnDefinition
colDefinitions = mkColumnDefinitions
  [ ColumnDefinition 0 "ERT"    200
  , ColumnDefinition 1 "S/C ID" 50
  , ColumnDefinition 2 "V/C ID" 50
  , ColumnDefinition 3 "VC FC"  50
  , ColumnDefinition 4 "MC FC"  50
  , ColumnDefinition 5 "DFH"    30
  , ColumnDefinition 6 "SRC"    50
  , ColumnDefinition 7 "Gap"    50
  , ColumnDefinition 8 "Qual"   50
  ]

