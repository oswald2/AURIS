{-# LANGUAGE OverloadedStrings
    , GeneralizedNewtypeDeriving
    , BangPatterns
    , NoImplicitPrelude
    , DataKinds
    , TemplateHaskell
#-}
module Data.MIB.PCF
    ( PCFentry(..)
    , loadFromFile
    , getPCFMap
    , getEndian
    , pcfName
    , pcfDescr
    , pcfPID
    , pcfUnit
    , pcfPTC
    , pcfPFC
    , pcfWidth
    , pcfValid
    , pcfRelated
    , pcfCateg
    , pcfNatur
    , pcfCurTx
    , pcfInter
    , pcfUscon
    , pcfDecim
    , pcfParVal
    , pcfSubSys
    , pcfValPar
    , pcfSpType
    , pcfCorr
    , pcfOBTID
    , pcfDARC
    , pcfEndian
    )
where

import           RIO
import qualified RIO.Vector                    as V
import           RIO.HashMap                   as HM

import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Types
import           Data.MIB.Load

import           General.Types



data PCFentry = PCFentry {
    _pcfName :: !ShortText,
    _pcfDescr :: !ShortText,
    _pcfPID :: !Word32,
    _pcfUnit :: !ShortText,
    _pcfPTC :: !Int,
    _pcfPFC :: !Int,
    _pcfWidth :: Maybe Word32,
    _pcfValid :: !ShortText,
    _pcfRelated :: !ShortText,
    _pcfCateg :: !Char,
    _pcfNatur :: !Char,
    _pcfCurTx :: !ShortText,
    _pcfInter :: CharDefaultTo "F",
    _pcfUscon :: CharDefaultTo "N",
    _pcfDecim :: Maybe Int,
    _pcfParVal :: !ShortText,
    _pcfSubSys :: !ShortText,
    _pcfValPar :: DefaultTo 1,
    _pcfSpType :: !ShortText,
    _pcfCorr :: CharDefaultTo "Y",
    _pcfOBTID :: Maybe Int,
    _pcfDARC :: Maybe Char,
    _pcfEndian :: Maybe Char
} deriving (Show, Read)
makeLenses ''PCFentry


getEndian :: PCFentry -> Endian
getEndian PCFentry { _pcfEndian = Just 'L' } = BiE
getEndian PCFentry { _pcfEndian = Just 'B' } = BiE
getEndian _ = BiE



instance Eq PCFentry where
    pcf1 == pcf2 = _pcfName pcf1 == _pcfName pcf2



instance FromRecord PCFentry where
    parseRecord v
        | V.length v >= 23
        = PCFentry
            <$> v
            .!  0
            <*> v
            .!  1
            <*> v
            .!  2
            <*> v
            .!  3
            <*> v
            .!  4
            <*> v
            .!  5
            <*> v
            .!  6
            <*> v
            .!  7
            <*> v
            .!  8
            <*> v
            .!  9
            <*> v
            .!  10
            <*> v
            .!  11
            <*> v
            .!  12
            <*> v
            .!  13
            <*> v
            .!  14
            <*> v
            .!  15
            <*> v
            .!  16
            <*> v
            .!  17
            <*> v
            .!  18
            <*> v
            .!  19
            <*> v
            .!  20
            <*> v
            .!  21
            <*> v
            .!  22
        | V.length v >= 19
        = PCFentry
            <$> v
            .!  0
            <*> v
            .!  1
            <*> v
            .!  2
            <*> v
            .!  3
            <*> v
            .!  4
            <*> v
            .!  5
            <*> v
            .!  6
            <*> v
            .!  7
            <*> v
            .!  8
            <*> v
            .!  9
            <*> v
            .!  10
            <*> v
            .!  11
            <*> v
            .!  12
            <*> v
            .!  13
            <*> v
            .!  14
            <*> v
            .!  15
            <*> v
            .!  16
            <*> v
            .!  17
            <*> v
            .!  18
            <*> pure (CharDefaultTo 'Y')
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
        | otherwise
        = mzero



fileName :: FilePath
fileName = "pcf.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector PCFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName



getPCFMap :: Vector PCFentry -> HashMap ShortText PCFentry
getPCFMap = V.foldl (\m e -> HM.insert (_pcfName e) e m) HM.empty
