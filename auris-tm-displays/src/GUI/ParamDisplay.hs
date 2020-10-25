module GUI.ParamDisplay
  ( ParamDisplay(..)
  , paramDispInsertValues
  , paramDispAddParameterDef
  , paramDispDestroy
  )
where


import           RIO

import           Data.TM.Parameter

import           GUI.GraphWidget

import           GUI.NameDescrTable             ( TableValue )


data ParamDisplay =
  GraphDisplay GraphWidget
  | ANDDisplay


paramDispInsertValues :: ParamDisplay -> Vector TMParameter -> IO ()
paramDispInsertValues (GraphDisplay gw) values =
  graphWidgetInsertParamValue gw values
paramDispInsertValues ANDDisplay _values = return ()


paramDispAddParameterDef :: ParamDisplay -> Vector TableValue -> IO ()
paramDispAddParameterDef (GraphDisplay gw) values =
  addParamFromSelector gw values
paramDispAddParameterDef ANDDisplay _values = return ()


paramDispDestroy :: ParamDisplay -> IO () 
paramDispDestroy (GraphDisplay gw) = graphWidgetDestroy gw 
paramDispDestroy ANDDisplay = return () 
