{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , GADTs
    , ExistentialQuantification
    , TemplateHaskell
#-}
module Data.PUS.PUSDfh
    ( DFH
    , PUSDfh(..)
    , stdType
    , stdSubType
    , stdSrcID
    , stdFlagAcceptance
    , stdFlagStartExec
    , stdFlagProgressExec
    , stdFlagExecComp
    )
where


import           RIO                     hiding ( Builder )

import           Control.Lens                   ( makeLenses
                                                , (.~)
                                                )
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits
import           ByteString.StrictBuilder
import           Data.Binary
import           Data.Aeson

import           Data.PUS.Types


-- | Secondary PUS headers are mission specific. Therefore we create
-- an existential type which adheres to this class.
class PUSDfh a where
    dfhBuilder :: a -> Builder
    dfhParser :: Parser a
    dfhType :: a -> PUSType
    dfhSetType :: a -> PUSType -> a
    dfhSubType :: a -> PUSSubType
    dfhSetSubType :: a -> PUSSubType -> a

-- | This is the existential type. It can contain any data structure,
-- which adheres to the 'PUSDfh' class
data DFH = forall a. (PUSDfh a, Show a) => DFH a

instance Show DFH where
    show (DFH x) = show x


instance PUSDfh DFH where
    dfhBuilder (DFH x) = dfhBuilder x
    dfhParser = dfhParser
    dfhType (DFH x) = dfhType x
    dfhSetType (DFH x) t = DFH (dfhSetType x t)
    dfhSubType (DFH x) = dfhSubType x
    dfhSetSubType (DFH x) st = DFH (dfhSetSubType x st)


data PUSEmptyDfh = PUSEmptyDfh
    deriving (Show, Read, Generic)

instance Binary PUSEmptyDfh
instance FromJSON PUSEmptyDfh
instance ToJSON PUSEmptyDfh where
    toEncoding = genericToEncoding defaultOptions

instance PUSDfh PUSEmptyDfh where
    dfhBuilder _ = mempty
    dfhParser = pure PUSEmptyDfh
    dfhType _ = mkPUSType 0
    dfhSubType _ = mkPUSSubType 0
    dfhSetType x _ = x
    dfhSetSubType x _ = x


data PUSStdHeader = PUSStdHeader {
    _stdType :: PUSType
    , _stdSubType :: PUSSubType
    , _stdSrcID :: !Word8
    , _stdFlagAcceptance :: !Bool
    , _stdFlagStartExec :: !Bool
    , _stdFlagProgressExec :: !Bool
    , _stdFlagExecComp :: !Bool
} deriving (Show, Read, Generic)
makeLenses ''PUSStdHeader

instance PUSDfh PUSStdHeader where
    dfhBuilder = stdBuilder
    dfhParser  = stdParser
    dfhType    = _stdType
    dfhSubType = _stdSubType
    dfhSetType x t = x & stdType .~ t
    dfhSetSubType x t = x & stdSubType .~ t

instance Binary PUSStdHeader
instance FromJSON PUSStdHeader
instance ToJSON PUSStdHeader where
    toEncoding = genericToEncoding defaultOptions


stdBuilder :: PUSStdHeader -> Builder
stdBuilder x =
    let b1 = 0x10 .|. if _stdFlagAcceptance x
            then 0x01
            else 0x00 .|. if _stdFlagStartExec x
                then 0x20
                else 0x00 .|. if _stdFlagProgressExec x
                    then 0x40
                    else 0x00 .|. if _stdFlagExecComp x then 0x80 else 0x00
    in  word8 b1
            <> pusTypeBuilder (_stdType x)
            <> pusSubTypeBuilder (_stdSubType x)
            <> word8 (_stdSrcID x)

stdParser :: Parser PUSStdHeader
stdParser = do
    b1 <- A.anyWord8
    t  <- pusTypeParser
    st <- pusSubTypeParser
    si <- A.anyWord8

    let fa  = b1 .&. 0x01 /= 0
        fs  = b1 .&. 0x02 /= 0
        fp  = b1 .&. 0x04 /= 0
        fe  = b1 .&. 0x08 /= 0
        hdr = PUSStdHeader { _stdType             = t
                           , _stdSubType          = st
                           , _stdSrcID            = si
                           , _stdFlagAcceptance   = fa
                           , _stdFlagStartExec    = fs
                           , _stdFlagProgressExec = fp
                           , _stdFlagExecComp     = fe
                           }

    pure hdr

