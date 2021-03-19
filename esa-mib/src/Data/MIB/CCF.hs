{-# LANGUAGE TemplateHaskell #-}
module Data.MIB.CCF
    ( CCFentry(..)
    , loadFromFile
    , ccfName
    , ccfDescr
    , ccfDescr2
    , ccfCType
    , ccfCritical
    , ccfPktID
    , ccfType
    , ccfSType
    , ccfApid
    , ccfNPars
    , ccfPlan
    , ccfExec
    , ccfIlScope
    , ccfIlStage
    , ccfSubSys
    , ccfHiPri
    , ccfMapID
    , ccfDefSet
    , ccfRApid
    , ccfAck
    , ccfSubschedID
    ) where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv

import           Data.MIB.Load
import           Data.MIB.Types




data CCFentry = CCFentry
    { _ccfName       :: !ShortText
    , _ccfDescr      :: !ShortText
    , _ccfDescr2     :: !ShortText
    , _ccfCType      :: !ShortText
    , _ccfCritical   :: CharDefaultTo "N"
    , _ccfPktID      :: !ShortText
    , _ccfType       :: Maybe Int
    , _ccfSType      :: Maybe Int
    , _ccfApid       :: Maybe Int
    , _ccfNPars      :: Maybe Int
    , _ccfPlan       :: CharDefaultTo "N"
    , _ccfExec       :: CharDefaultTo "Y"
    , _ccfIlScope    :: CharDefaultTo "N"
    , _ccfIlStage    :: CharDefaultTo "C"
    , _ccfSubSys     :: Maybe Int 
    , _ccfHiPri      :: CharDefaultTo "N"
    , _ccfMapID      :: Maybe Int
    , _ccfDefSet     :: !ShortText
    , _ccfRApid      :: Maybe Int
    , _ccfAck        :: Maybe Int
    , _ccfSubschedID :: Maybe Int
    }
    deriving (Show, Read)
makeLenses ''CCFentry



instance Eq CCFentry where
    c1 == c2 = c1 ^. ccfName == c2 ^. ccfName

instance FromRecord CCFentry where
    parseRecord = genericParse (>= 21) CCFentry


fileName :: FilePath
fileName = "ccf.dat"

loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector CCFentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


