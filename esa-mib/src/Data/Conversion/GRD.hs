module Data.Conversion.GRD
  ( loadGRDs
  )
where


import           RIO                     hiding ( to )

import           Control.Lens

import qualified RIO.Vector                    as V
import qualified RIO.Map                       as M

import           Data.Display.Graphical
import           Data.Text.Short                ( ShortText )

import           Data.MIB.GPC                  as GPC
import           Data.MIB.GPF                  as GPF
import           Data.MIB.Types

import           Data.Colour
import           Data.Colour.Names


charToColor :: Char -> GRDColour
charToColor '1' = GRDColour $ opaque green
charToColor '2' = GRDColour $ opaque blue
charToColor '3' = GRDColour $ opaque cyan
charToColor '4' = GRDColour $ opaque red
charToColor '5' = GRDColour $ opaque yellow
charToColor '6' = GRDColour $ opaque magenta
charToColor '7' = GRDColour $ opaque white
charToColor _   = GRDColour $ opaque blue

convertRaw :: CharDefaultTo a -> PrintRaw
convertRaw x = case getDefaultChar x of
  'R' -> PrRaw
  _   -> PrCalibrated


convertSymbol :: CharDefaultTo a -> Symbol
convertSymbol x = case getDefaultChar x of
  '0' -> NoSymbol
  '1' -> XSymbol
  '2' -> PlusSymbol
  '3' -> CircleSymbol
  '4' -> StarSymbol
  '5' -> RhombSymbol
  '6' -> SquareSymbol
  _   -> NoSymbol

convertLineType :: CharDefaultTo a -> LineType
convertLineType x = case getDefaultChar x of
  '0' -> NoLine
  '1' -> SolidLine
  '2' -> LongDash
  '3' -> Dash
  '4' -> ShortDash
  '5' -> DashShortDash
  _   -> NoLine


convertGPC :: GPCentry -> GRDParameter
convertGPC GPCentry {..} = GRDParameter
  { _grdpName      = _gpcParamName
  , _grdpRaw       = convertRaw _gpcRaw
  , _grdpPlotColor = charToColor _gpcColor
  , _grdpSymbol    = convertSymbol _gpcSymbol
  , _grdpLineType  = convertLineType _gpcLine
  }


convertGPF :: GPFentry -> [GRDParameter] -> GRD
convertGPF GPFentry {..} params = GRD
  { _grdName      = _gpfName
  , _grdHeader    = _gpfHead
  , _grdScroll    = getDefaultChar _gpfScroll /= 'N'
  , _grdHardCopy  = getDefaultChar _gpfHCopy /= 'N'
  , _grdDays      = _gpfDays
  , _grdHours     = _gpfHours
  , _grdMinutes   = _gpfMinutes
  , _grdAxesColor = charToColor _gpfAxesColor
  , _grdXTick     = fromIntegral _gpfXTick
  , _grdYTick     = fromIntegral _gpfYTick
  , _grdXGrid     = fromIntegral _gpfXGrid
  , _grdYGrid     = fromIntegral _gpfYGrid
  , _grdParams    = V.fromList params
  }


loadGRDs
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => FilePath
  -> m (Either Text (Map ShortText GRD))
loadGRDs path = do
  gpcs' <- GPC.loadFromFile path
  case gpcs' of
    Left  err  -> return (Left err)
    Right gpcs -> do
      gpfs' <- GPF.loadFromFile path
      case gpfs' of
        Left  err  -> return (Left err)
        Right gpfs -> do
          let filt gpf x = _gpfName gpf == _gpcName x
              params gpf =
                gpcs ^.. folded . filtered (filt gpf) . to convertGPC
              
              f gpf = 
                let p = params gpf in 
                (_gpfName gpf, convertGPF gpf p)
          return (Right (M.fromList . fmap f . toList $ gpfs))

