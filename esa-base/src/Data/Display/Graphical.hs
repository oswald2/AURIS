module Data.Display.Graphical
  ( PrintRaw(..)
  , Symbol(..)
  , LineType(..)
  , GRDParameter(..)
  , GRD(..)
  , GRDColour(..)
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Aeson
import           Codec.Serialise               as S

import           General.Types                  ( )


data PrintRaw = PrRaw | PrCalibrated
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise PrintRaw
instance FromJSON PrintRaw
instance ToJSON PrintRaw where
  toEncoding = genericToEncoding defaultOptions


data Symbol =
  NoSymbol
  | XSymbol
  | PlusSymbol
  | CircleSymbol
  | StarSymbol
  | RhombSymbol
  | SquareSymbol
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise Symbol
instance FromJSON Symbol 
instance ToJSON Symbol where 
  toEncoding = genericToEncoding defaultOptions 


data LineType =
  NoLine
  | SolidLine
  | LongDash
  | Dash
  | ShortDash
  | DashShortDash
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise LineType
instance FromJSON LineType 
instance ToJSON LineType where 
  toEncoding = genericToEncoding defaultOptions 


newtype GRDColour = GRDColour (AlphaColour Double)
  deriving (Show, Generic)



data EncColor = EncColor !Double !Double !Double !Double
  deriving Generic

instance Serialise EncColor
instance FromJSON EncColor 
instance ToJSON EncColor where
  toEncoding = genericToEncoding defaultOptions


{-# INLINABLE pureColour #-}
pureColour :: AlphaColour Double -> Colour Double
pureColour ac = darken (recip a) (ac `Data.Colour.over` black)
  where a = alphaChannel ac


{-# INLINABLE toEncColor #-}
toEncColor :: GRDColour -> EncColor
toEncColor (GRDColour c) =
  let rgb = toSRGB (pureColour c)
  in  EncColor (channelRed rgb)
               (channelGreen rgb)
               (channelBlue rgb)
               (alphaChannel c)

{-# INLINABLE fromEncColor #-}
fromEncColor :: EncColor -> GRDColour
fromEncColor (EncColor red green blue alpha) = 
   let c = sRGB red green blue 
   in
   GRDColour (withOpacity c alpha)


instance Serialise GRDColour where
  encode = S.encode . toEncColor
  decode = fromEncColor <$> S.decode

instance FromJSON GRDColour where 
  parseJSON x = fromEncColor <$> parseJSON x

instance ToJSON GRDColour where 
  toJSON = toJSON . toEncColor 
  toEncoding = toEncoding . toEncColor

data GRDParameter = GRDParameter {
  _grdpName :: !ShortText
  , _grdpRaw :: !PrintRaw
  , _grdpPlotColor :: GRDColour
  , _grdpSymbol :: !Symbol
  , _grdpLineType :: !LineType
  } deriving (Show, Generic)

instance Serialise GRDParameter
instance FromJSON GRDParameter 
instance ToJSON GRDParameter where
  toEncoding = genericToEncoding defaultOptions


data GRD = GRD {
    _grdName :: !ShortText
    , _grdHeader :: !ShortText
    , _grdScroll :: !Bool
    , _grdHardCopy :: !Bool
    , _grdDays :: !Int
    , _grdHours :: !Int
    , _grdMinutes :: !Int
    , _grdAxesColor :: GRDColour
    , _grdXTick :: !Int
    , _grdYTick :: !Int
    , _grdXGrid :: !Int
    , _grdYGrid :: !Int
    , _grdParams :: Vector GRDParameter
  } deriving (Show, Generic)


instance Serialise GRD
instance FromJSON GRD 
instance ToJSON GRD where
  toEncoding = genericToEncoding defaultOptions
