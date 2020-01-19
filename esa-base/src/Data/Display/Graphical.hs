module Data.Display.Graphical
  ( PrintRaw(..)
  , Symbol(..)
  , LineType(..)
  , GRDParameter(..)
  , GRD(..)
  )
where

import           RIO

import           Data.Text.Short                ( ShortText )
import           Data.Colour


data PrintRaw = PrRaw | PrCalibrated
  deriving (Eq, Ord, Enum, Show, Generic)

data Symbol =
  NoSymbol
  | XSymbol
  | PlusSymbol
  | CircleSymbol
  | StarSymbol
  | RhombSymbol
  | SquareSymbol
  deriving (Eq, Ord, Enum, Show, Generic)

data LineType =
  NoLine
  | SolidLine
  | LongDash
  | Dash
  | ShortDash
  | DashShortDash
  deriving (Eq, Ord, Enum, Show, Generic)


data GRDParameter = GRDParameter {
  _grdpName :: !ShortText
  , _grdpRaw :: !PrintRaw
  , _grdpPlotColor :: AlphaColour Double
  , _grdpSymbol :: !Symbol
  , _grdpLineType :: !LineType
  } deriving (Show, Generic)


data GRD = GRD {
    _grdName :: !ShortText
    , _grdHeader :: !ShortText
    , _grdScroll :: !Bool
    , _grdHardCopy :: !Bool
    , _grdDays :: !Int
    , _grdHours :: !Int
    , _grdMinutes :: !Int
    , _grdAxesColor :: AlphaColour Double
    , _grdXTick :: !Int
    , _grdYTick :: !Int
    , _grdXGrid :: !Int
    , _grdYGrid :: !Int
    , _grdParams :: Vector GRDParameter
  } deriving (Show, Generic)



