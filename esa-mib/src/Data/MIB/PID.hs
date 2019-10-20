{-# LANGUAGE
    TemplateHaskell
#-}
module Data.MIB.PID
    ( PIDentry(..)
    , DefaultTime(..)
    , loadFromFile
    , getPidTime
    , pidType
    , pidSubType
    , pidAPID
    , pidP1Val
    , pidP2Val
    , pidSPID
    , pidDescr
    , pidUnit
    , pidTPSD
    , pidDfhSize
    , pidTime
    , pidInter
    , pidValid
    , pidCheck
    , pidEvent
    , pidEventID
    )
where

import           RIO
import           Control.Lens                   ( makeLenses )

import           Data.Text.Short                ( ShortText )
import           Data.Csv
import qualified RIO.Vector                    as V

import           Data.MIB.Load



newtype DefaultTime = DefTime {defaultTimeVal :: Char}  deriving (Eq, Ord, Show, Read)

instance FromField DefaultTime where
    parseField s =
        DefTime . fromMaybe 'N' <$> (parseField s :: Parser (Maybe Char))



data PIDentry = PIDentry {
    _pidType :: !Int,
    _pidSubType :: !Int,
    _pidAPID :: !Int,
    _pidP1Val :: !Int,
    _pidP2Val :: !Int,
    _pidSPID :: !Word32,
    _pidDescr :: !ShortText,
    _pidUnit :: !ByteString,
    _pidTPSD :: !Int,
    _pidDfhSize :: !Int,
    _pidTime :: !DefaultTime,
    _pidInter :: !ShortText,
    _pidValid :: !ShortText,
    _pidCheck :: Maybe Int,
    _pidEvent :: !ShortText,
    _pidEventID :: !ShortText
} deriving (Show, Read)
makeLenses ''PIDentry

instance Eq PIDentry where
    (PIDentry t1 st1 ap1 p11 p12 _ _ _ _ _ _ _ _ _ _ _) == (PIDentry t2 st2 ap2 p21 p22 _ _ _ _ _ _ _ _ _ _ _)
        = (t1 == t2)
            && (st1 == st2)
            && (ap1 == ap2)
            && (p11 == p21)
            && (p12 == p22)




getPidTime :: PIDentry -> Char
getPidTime = defaultTimeVal . _pidTime



instance FromRecord PIDentry where
    parseRecord v
        | V.length v >= 16
        = PIDentry
            <$> v
            .!  0
            <*> v
            .!  1
            <*> v
            .!  2
            <*> v
            .!  3
            <*> v
            .!  4
            <*> v
            .!  5
            <*> v
            .!  6
            <*> v
            .!  7
            <*> v
            .!  8
            <*> v
            .!  9
            <*> v
            .!  10
            <*> v
            .!  11
            <*> v
            .!  12
            <*> v
            .!  13
            <*> v
            .!  14
            <*> v
            .!  15
        | otherwise
        = mzero




fileName :: FilePath
fileName = "pid.dat"

loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => FilePath
    -> m (Either Text (Vector PIDentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


