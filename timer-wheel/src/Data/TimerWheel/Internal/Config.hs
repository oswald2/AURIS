module Data.TimerWheel.Internal.Config
  ( Config (..),
  )
where

import Data.Fixed (E6, Fixed)
import GHC.Generics (Generic)

-- | Timer wheel config.
--
-- * @spokes@ must be ∈ @(0, maxBound]@
-- * @resolution@ must ∈ @(0, ∞]@
data Config = Config
  { -- | Spoke count.
    spokes :: Int,
    -- | Resolution, in seconds.
    resolution :: Fixed E6
  }
  deriving stock (Generic, Show)
