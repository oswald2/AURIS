module Data.Conversion.TCParameter
    ( convertParam
    ) where

import           RIO
import qualified Data.Text.Short               as ST

import           Data.MIB.CPC

import           Data.PUS.Parameter
import           Data.PUS.Value



convertParam :: CPCentry -> Parameter
convertParam e@CPCentry {..} = Parameter { 
    _paramName = ST.toText _cpcName 
    , _paramValue = determineValue e
    }



determineValue :: CPCentry -> Value 
determineValue CPCentry {..} = 
    initialValue BiE (PTC _cpcPTC) (PFC _cpcPFC)
    -- TODO: set default value, parse from _cpcDefVal