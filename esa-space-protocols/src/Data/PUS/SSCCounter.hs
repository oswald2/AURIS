module Data.PUS.SSCCounter
  ( SSCCounter
  , SSCCounterMap
  , initialSSCCounterMap
  , getNext
  , getNextSTM
  , getNextSSC
  )
where

import           RIO
import qualified RIO.HashMap                   as HM

import General.PUSTypes ( mkSSC, nextSSC, SSC )
import General.APID ( APID )



type SSCCounter = TVar SSC
type SSCCounterMap = HashMap APID SSCCounter


initialSSCCounterMap :: HashMap APID SSCCounter
initialSSCCounterMap = HM.empty

getNextSTM :: SSCCounter -> STM SSC
getNextSTM ctr = do
  val <- readTVar ctr
  writeTVar ctr (nextSSC val)
  return val


getNext :: (MonadIO m) => SSCCounter -> m SSC
getNext ctr = atomically (getNextSTM ctr)



getNextSSC :: (MonadIO m) => SSCCounterMap -> APID -> m (SSCCounterMap, SSC)
getNextSSC hm apid = do
  case HM.lookup apid hm of 
    Nothing -> do
      let ssc = mkSSC 0 
      v <- newTVarIO ssc 
      let !newHM = HM.insert apid v hm 
      return (newHM, ssc)
    Just ctr -> do 
      ssc <- getNext ctr 
      return (hm, ssc)

