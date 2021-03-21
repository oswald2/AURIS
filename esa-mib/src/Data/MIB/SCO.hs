module Data.MIB.SCO
    ( SCOentry(..)
    , loadFromFile
    , getSCOSet
    , getSCOMap
    ) where

import           RIO

import qualified RIO.Vector                    as V
import qualified RIO.HashMap                   as HM
import qualified RIO.Set                       as S
import           Data.Text.Short                ( ShortText )
import           Data.Csv                       ( FromRecord(..) )

import           Data.MIB.Load                  ( loadFromFileGen
                                                , GenericParse(genericParse)
                                                )


data SCOentry = SCOentry
    { _scoLink :: !ShortText
    , _scoSCOE :: !ShortText
    , _scoPort :: !Word16
    , _scoFlag :: Maybe Int
    }
    deriving Show

instance Eq SCOentry where
    f1 == f2 = (_scoLink f1 == _scoLink f2) && (_scoSCOE f1 == _scoSCOE f2)

instance Ord SCOentry where
    compare f1 f2 = case compare (_scoSCOE f1) (_scoSCOE f2) of
        LT -> LT
        GT -> GT
        EQ -> compare (_scoLink f1) (_scoLink f2)


instance FromRecord SCOentry where
    parseRecord = genericParse (== 4) SCOentry


fileName :: FilePath
fileName = "sco.dat"


loadFromFile
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Either Text (Vector SCOentry))
loadFromFile mibPath = loadFromFileGen mibPath fileName


{-# INLINABLE getSCOSet #-}
getSCOSet :: Vector SCOentry -> Set SCOentry
getSCOSet vec = V.foldl' (\m e -> S.insert e m) S.empty vec


{-# INLINABLE getSCOMap #-}
getSCOMap :: Vector SCOentry -> HashMap ShortText SCOentry
getSCOMap vec = V.foldl' (\m e -> HM.insert (_scoLink e) e m) HM.empty vec
