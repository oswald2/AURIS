module GUI.ParamDisplay
    ( ParamDisplay(..)
    , paramDispInsertValues
    , paramDispAddParameterDef
    , paramDispDestroy
    ) where


import           RIO
import qualified RIO.Text                      as T

import           Data.TM.Parameter

import           GUI.ANDWidget
import           GUI.GraphWidget

import           GUI.NameDescrTable             ( TableValue )


data ParamDisplay =
  GraphDisplay GraphWidget
  | ANDDisplay ANDWidget


paramDispInsertValues :: ParamDisplay -> Vector TMParameter -> IO ()
paramDispInsertValues (GraphDisplay gw) values =
    graphWidgetInsertParamValue gw values
paramDispInsertValues (ANDDisplay aw) values =
    andWidgetUpdateParamValue aw values


paramDispAddParameterDef :: ParamDisplay -> Vector TableValue -> IO ()
paramDispAddParameterDef (GraphDisplay gw) values =
    addParamFromSelector gw values
paramDispAddParameterDef (ANDDisplay aw) values = do 
    traceM $ "paramDispAddParameterDef: adding values "<> T.pack (show values) 
    andWidgetAddParamFromSelector aw values



paramDispDestroy :: ParamDisplay -> IO ()
paramDispDestroy (GraphDisplay gw) = graphWidgetDestroy gw
paramDispDestroy (ANDDisplay   aw) = andWidgetDestroy aw
