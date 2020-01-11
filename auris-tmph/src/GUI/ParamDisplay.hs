module GUI.ParamDisplay
  ( ParamDisplay(..)
  , paramDispInsertValues
  )
where


import           RIO

import           Data.TM.Parameter

import           GUI.Graph


data ParamDisplay =
  GraphDisplay GraphWidget
  | ANDDisplay


paramDispInsertValues :: ParamDisplay -> Vector TMParameter -> IO ()
paramDispInsertValues (GraphDisplay gw) values =
  graphInsertParamValue gw values
paramDispInsertValues ANDDisplay _values = return ()
