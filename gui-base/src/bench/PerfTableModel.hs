{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
    , TypeApplications
    , ScopedTypeVariables
#-}
module Main where

import           RIO
import           RIO.List
import qualified Data.Text.IO                  as T
import qualified RIO.ByteString                as B
import qualified Data.Vector.Storable          as V
import qualified Data.Vector.Storable.Mutable  as V


import           Model.ScrollingTableModel

import           Criterion.Main


main :: IO ()
main = do
  defaultMain
    [ bgroup
        "tableModelAddValue"
        [ bench "TableModel" $ nfIO $ vectorModel 10000
        , bench "MTableModel" $ nfIO $ mvectorModel 10000
        ]
    ]



vectorModel size = do
  (model :: VectorTableModel Int)  <- tableModelNew 
  let action = do 
        tableModelAddValue model 500 513456
        let idx = [0..499]
        mapM_ (tableModelIndexUnlocked model) idx 
  sequence_ $ replicate size action


mvectorModel size = do
  (model :: MVectorTableModel Int) <- tableModelNew 
  let action = do 
        tableModelAddValue model 500 513456
        let idx = [0..499]
        mapM_ (tableModelIndexUnlocked model) idx 
  sequence_ $ replicate size action
