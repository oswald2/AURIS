module GUI.TMPParamTable
  ( colDefinitions
  )
where

import           RIO

import           Model.ScrollingTableModel




colDefinitions :: Vector ColumnDefinition
colDefinitions = mkColumnDefinitions
  [ ColumnDefinition 0 "Parameter" 100
  , ColumnDefinition 1 "Timestamp" 200
  , ColumnDefinition 2 "Raw Value" 100
  ]

