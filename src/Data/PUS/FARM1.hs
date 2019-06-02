{-# LANGUAGE GeneralizedNewtypeDeriving
    , BangPatterns
    , NoImplicitPrelude
    , TemplateHaskell
#-}
module Data.PUS.FARM1
    ( FARMState
    , FarmState(..)
    , initialFARMState
    , acceptTCFrame
    , rejectTCFrame
    , acceptBDFrame
    , acceptUnlockDirective
    , acceptSetVR
    , getCLCW
    , getTC
    , setVR
    , toggleWait
    , toggleRetransmit
    , toggleLockout
    , nextFarmState
    , toggleNoRF
    , toggleNoBitLock
    , farmState
    , farmLockout
    , farmRetransmit
    , farmWait
    , farmVR
    , farmNoRF
    , farmNoBitlock
    , farmBCounter
    , farmSlidingWinWidth
    , farmSlidingPosWidth
    , farmSlidingNegWidth
    )
where

import           RIO
import           RIO.List

import           Control.Lens                   ( makeLenses )
import           Control.Lens.Setter

import           Data.PUS.TCFrameTypes
import           Data.PUS.CLCW
import           Data.PUS.Types

import qualified Data.ByteString.Lazy          as B



data FarmState = Open
               | Wait
               | Lockout
                 deriving (Eq, Ord, Enum, Show)


data FARMState = FARMState {
  _farmState :: !FarmState,
  _farmLockout :: !Bool,
  _farmRetransmit :: !Bool,
  _farmWait :: !Bool,
  _farmVR :: !Word8,
  _farmBCounter :: !Word8,
  _farmSlidingWinWidth :: !Word8,
  _farmSlidingPosWidth :: !Word8,
  _farmSlidingNegWidth :: !Word8,
  _farmBuffer :: ![TCTransferFrame],
  _farmNoRF :: !Bool,
  _farmNoBitlock :: !Bool
  } deriving (Show)

makeLenses ''FARMState


initialFARMState :: FARMState
initialFARMState = FARMState Open False False False 0 0 10 5 5 [] False False


_checkSlidingWinWidth :: Word8 -> Bool
_checkSlidingWinWidth w = (2 < w) && (w < 254) && even w


checkForLockout :: FARMState -> Word8 -> Bool
checkForLockout state ns =
    let vr     = _farmVR state
        pw     = _farmSlidingPosWidth state
        nw     = _farmSlidingNegWidth state
        res1   = ns > vr + pw - 1
        res2   = ns < vr - nw
        result = res1 && res2
    in  result


checkForAccept :: FARMState -> Word8 -> Bool
checkForAccept state ns = (ns == _farmVR state)


checkSpace :: FARMState -> Bool
checkSpace state = (null (_farmBuffer state))

isFrameAvailable :: FARMState -> Bool
isFrameAvailable = not . checkSpace



setState :: FarmState -> FARMState -> FARMState
setState st state = farmState .~ st $ state

setVR :: Word8 -> FARMState -> FARMState
setVR vr state = farmVR .~ vr $ state

setRetransmitAndWait :: FARMState -> FarmState -> FARMState
setRetransmitAndWait state newSt =
    state { _farmState = newSt, _farmRetransmit = True, _farmWait = True }

setRetransmit :: FARMState -> FARMState
setRetransmit state = farmRetransmit .~ True $ state

toggleWait :: FARMState -> FARMState
toggleWait state = farmWait %~ not $ state

toggleRetransmit :: FARMState -> FARMState
toggleRetransmit state = farmRetransmit %~ not $ state

toggleLockout :: FARMState -> FARMState
toggleLockout state = farmLockout %~ not $ state

nextFarmState :: FARMState -> FARMState
nextFarmState state =
    let nextSt Open    = Wait
        nextSt Wait    = Lockout
        nextSt Lockout = Open
    in  farmState %~ nextSt $ state

toggleNoRF :: FARMState -> FARMState
toggleNoRF state = farmNoRF %~ not $ state

toggleNoBitLock :: FARMState -> FARMState
toggleNoBitLock state = farmNoBitlock %~ not $ state




addFrame :: TCTransferFrame -> FARMState -> FARMState
addFrame frame state =
    state & farmBuffer .~ [frame] & farmRetransmit .~ False & farmVR +~ 1

getFrame :: FARMState -> (FARMState, Maybe TCTransferFrame)
getFrame state =
    let res = case isFrameAvailable state of
            True  -> headMaybe $ state ^. farmBuffer
            False -> Nothing
    in  ((farmBuffer .~ []) state, res)



checkForRetransmit :: FARMState -> Word8 -> Bool
checkForRetransmit state ns =
    let !vr     = _farmVR state
        !pw     = _farmSlidingPosWidth state
        !res1   = ns > vr
        !res2   = ns <= vr + pw - 1
        !result = (res1 && res2)
    in  result

_checkForDiscard :: FARMState -> Word8 -> Bool
_checkForDiscard state ns =
    let !vr     = _farmVR state
        !nw     = _farmSlidingNegWidth state
        !res1   = ns < vr
        !res2   = ns >= vr - nw
        !result = (res1 && res2)
    in  result

nextFarmBCounter :: Word8 -> Word8
nextFarmBCounter !bc = let !res = (bc + 1) `quot` 4 in res

acceptBD :: TCTransferFrame -> FARMState -> FARMState
acceptBD frame state =
    state & farmBuffer .~ [frame] & farmBCounter %~ nextFarmBCounter

incFarmBCounter :: FARMState -> FARMState
incFarmBCounter = farmBCounter %~ nextFarmBCounter


setLockout :: FARMState -> FARMState
setLockout state = state & farmState .~ Lockout & farmLockout .~ True

clearRetrans :: FARMState -> FARMState
clearRetrans = farmRetransmit .~ False

clearWait :: FARMState -> FARMState
clearWait = farmWait .~ False

clearLockout :: FARMState -> FARMState
clearLockout = farmLockout .~ False




acceptTCFrame :: TCTransferFrame -> FARMState -> FARMState
acceptTCFrame frame state =
    let ns       = frame ^. tcFrameSeq
        newState = case checkForAccept state ns of
            True -> case checkSpace state of
                True -> case _farmState state of
                    Open -> addFrame frame state
                    _    -> state
                False -> case _farmState state of
                    Open -> setRetransmitAndWait state Wait
                    _    -> state
            False -> case checkForRetransmit state ns of
                True -> case _farmState state of
                    Open -> setRetransmit state
                    _    -> state
                False -> case checkForLockout state ns of
                    True -> case _farmState state of
                        Open -> setLockout state
                        Wait -> setLockout state
                        _    -> state
                    False -> state
    in  newState

acceptBDFrame :: TCTransferFrame -> FARMState -> FARMState
acceptBDFrame frame = acceptBD frame


rejectTCFrame :: TCTransferFrame -> B.ByteString -> FARMState -> FARMState
rejectTCFrame _hdr _pl state = state


acceptUnlockDirective :: FARMState -> FARMState
acceptUnlockDirective state = case _farmState state of
    Open -> (((setState Open) . clearRetrans . incFarmBCounter) state)
    Wait ->
        (((setState Open) . clearWait . clearRetrans . incFarmBCounter) state)
    Lockout ->
        (( (setState Open)
         . clearLockout
         . clearWait
         . clearRetrans
         . incFarmBCounter
         )
            state
        )


acceptSetVR :: Word8 -> FARMState -> FARMState
acceptSetVR vr state = case _farmState state of
    Open ->
        (((setState Open) . (setVR vr) . clearRetrans . incFarmBCounter) state)
    Wait ->
        (( (setState Open)
         . (setVR vr)
         . clearWait
         . clearRetrans
         . incFarmBCounter
         )
            state
        )
    Lockout -> (incFarmBCounter state)


getCLCW :: VCID -> FARMState -> CLCW
getCLCW vcid state = createCLCW vcid
                                (_farmVR state)
                                (_farmNoRF state)
                                (_farmNoBitlock state)
                                (_farmLockout state)
                                (_farmWait state)
                                (_farmRetransmit state)
                                (_farmBCounter state)


getTC :: FARMState -> (FARMState, (Maybe TCTransferFrame, CLCW))
getTC state =
    let
        vcid Nothing  = 0
        vcid (Just f) = f ^. tcFrameVCID

        res@(_newState, (_frame, _clcw)) = case _farmState state of
            Open ->
                let (st, fr) = getFrame state
                in  (st, (fr, getCLCW (vcid fr) state))
            Wait ->
                let (st, fr) = getFrame . clearWait $ state
                in  ( st
                    , ( fr
                      , createCLCW (vcid fr)
                                   (_farmVR state)
                                   (_farmNoRF state)
                                   (_farmNoBitlock state)
                                   (_farmLockout state)
                                   False
                                   (_farmRetransmit state)
                                   (_farmBCounter state)
                      )
                    )
            Lockout ->
                let (st, fr) = getFrame . clearWait $ state
                in  ( st
                    , ( fr
                      , createCLCW (vcid fr)
                                   (_farmVR state)
                                   (_farmNoRF state)
                                   (_farmNoBitlock state)
                                   (_farmLockout state)
                                   False
                                   (_farmRetransmit state)
                                   (_farmBCounter state)
                      )
                    )
    in
        res


