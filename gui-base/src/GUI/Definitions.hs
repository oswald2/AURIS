module GUI.Definitions
  ( defMaxRowTM
  )
where


import           RIO

-- | The maximum number of rows in a TM display in live mode
defMaxRowTM :: Int32
defMaxRowTM = 200
