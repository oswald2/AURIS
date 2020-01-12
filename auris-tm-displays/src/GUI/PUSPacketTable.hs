{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
#-}
module GUI.PUSPacketTable
  ( colDefinitions
  )
where

import           RIO

import           Model.ScrollingTableModel





colDefinitions :: Vector ColumnDefinition
colDefinitions = mkColumnDefinitions
  [ ColumnDefinition 0 "Generation Time" 200
  , ColumnDefinition 1 "ERT"             200
  , ColumnDefinition 2 "APID"            60
  , ColumnDefinition 3 "T"               30
  , ColumnDefinition 4 "ST"              30
  , ColumnDefinition 5 "SSC"             50
  , ColumnDefinition 6 "Data"            800
  ]


