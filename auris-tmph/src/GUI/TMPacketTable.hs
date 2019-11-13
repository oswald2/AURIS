module GUI.TMPacketTable
  ( colDefinitions
  )
where

import           RIO

import           Model.ScrollingTableModel




colDefinitions :: Vector ColumnDefinition
colDefinitions = mkColumnDefinitions
  [ ColumnDefinition 0 "SPID"            100
  , ColumnDefinition 1 "Mnemonic"        100
  , ColumnDefinition 2 "Description"     330
  , ColumnDefinition 3 "Generation Time" 200
  , ColumnDefinition 4 "ERT"             200
  , ColumnDefinition 5 "APID"            60
  , ColumnDefinition 6 "T"               30
  , ColumnDefinition 7 "ST"              30
  , ColumnDefinition 8 "SSC"             50
  , ColumnDefinition 9 "VC"              30
  ]

