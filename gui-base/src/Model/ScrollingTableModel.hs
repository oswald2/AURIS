{-# LANGUAGE 
    OverloadedStrings
    , BangPatterns
    , NoImplicitPrelude
    , FlexibleInstances
#-}
module Model.ScrollingTableModel
  ( ToCellText(..)
  , modelMaxRows
  )
where


import           RIO
import qualified RIO.Text                      as T



modelMaxRows :: Int
modelMaxRows = 20



class ToCellText a where
    toCellText :: Maybe a -> Int -> Text


