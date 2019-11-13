module GUI.TMFrameTable
  ( colDefinitions
  )
where

import           RIO

import           Model.ScrollingTableModel




colDefinitions :: Vector ColumnDefinition
colDefinitions = mkColumnDefinitions
  [ ColumnDefinition 0 "S/C ID" 50
  , ColumnDefinition 1 "V/C ID" 50
  , ColumnDefinition 2 "VC FC"  50
  , ColumnDefinition 3 "MC FC"  50
  , ColumnDefinition 4 "DFH"    30
  ]

