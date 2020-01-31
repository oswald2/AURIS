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
import           RIO.Partial                    ( read )

import           Data.Text.Short                ( ShortText )
import           Data.Colour

import           Codec.Serialise

import           General.Types ()


data PrintRaw = PrRaw | PrCalibrated
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise PrintRaw


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


data LineType =
  NoLine
  | SolidLine
  | LongDash
  | Dash
  | ShortDash
  | DashShortDash
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialise LineType


newtype GRDColour = GRDColour (AlphaColour Double)
  deriving (Show, Generic)


instance Serialise GRDColour where
  encode x = encode (show x)
  decode = GRDColour . read <$> decode


data GRDParameter = GRDParameter {
  _grdpName :: !ShortText
  , _grdpRaw :: !PrintRaw
  , _grdpPlotColor :: GRDColour
  , _grdpSymbol :: !Symbol
  , _grdpLineType :: !LineType
  } deriving (Show, Generic)

instance Serialise GRDParameter


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
