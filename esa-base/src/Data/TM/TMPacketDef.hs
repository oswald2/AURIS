{-# LANGUAGE
    TemplateHaskell
#-}
module Data.TM.TMPacketDef
    ( TMPacketDef(..)
    , tmpdSPID
    , tmpdType
    , tmpdSubType
    , tmpdApid
    , tmpdPI1Val
    , tmpdPI2Val
    , tmpdName
    )
where

import           RIO
import           Data.Text.Short                ( ShortText )

import           Control.Lens                   ( makeLenses )

import           General.PUSTypes
import           General.APID

--import           Data.TM.PIVals



data TMPacketDef = TMPacketDef {
    _tmpdSPID :: SPID
    , _tmpdName :: ShortText
    , _tmpdType :: PUSType
    , _tmpdSubType :: PUSSubType
    , _tmpdApid :: APID
    , _tmpdPI1Val :: Int
    , _tmpdPI2Val :: Int
    } deriving(Show, Generic)
makeLenses ''TMPacketDef
