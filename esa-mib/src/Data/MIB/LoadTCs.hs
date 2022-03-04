module Data.MIB.LoadTCs
    ( loadTCs
    ) where

import           RIO

import           Data.HashTable.ST.Basic        ( IHashTable )

import           Data.Text.Short                ( ShortText )

import           Control.Monad.Except

import           Data.TC.TCDef
import           Data.TC.TCParameterDef

import           Data.MIB.CCA                  as CCA
import           Data.MIB.CCF                  as CCF
import           Data.MIB.CCS                  as CCS
import           Data.MIB.CDF                  as CDF
import           Data.MIB.CPC                  as CPC
import           Data.MIB.CVP                  as CVP
import           Data.MIB.CVS                  as CVS
import           Data.MIB.PAF                  as PAF
import           Data.MIB.PAS                  as PAS
import           Data.MIB.PRF                  as PRF
import           Data.MIB.PRV                  as PRV
import           Data.MIB.SCO                  as SCO
import           Data.MIB.TCD                  as TCD

import           Data.Conversion.TCCalibration
import           Data.Conversion.TCs

import           General.Time


loadTCs
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => Epoch
    -> CorrelationCoefficients
    -> ShortText
    -> FilePath
    -> m
           ( Either
                 Text
                 ( [Text]
                 , IHashTable ShortText TCDef
                 , HashMap ShortText TCParameterDef
                 )
           )
loadTCs epoch coeff defaultConnName mibPath = do
    runExceptT $ do
        prfMap <- PRF.loadFromFile mibPath >>= liftEither
        prvMap <- PRV.loadFromFile mibPath >>= liftEither
        ccas   <- CCA.loadFromFile mibPath >>= liftEither
        ccss   <- CCS.loadFromFile mibPath >>= liftEither
        pafs   <- PAF.loadFromFile mibPath >>= liftEither
        pass   <- PAS.loadFromFile mibPath >>= liftEither

        let (tErrs, textCalibs) = convertTcTextCalib pafs (getPASMap pass)
            (nErrs, numCalibs ) = convertTcNumCalib ccas (getCCSMap ccss)

        cdfs <- CDF.loadFromFile mibPath >>= liftEither
        cpcs <- CPC.loadFromFile mibPath >>= liftEither
        ccfs <- CCF.loadFromFile mibPath >>= liftEither

        cvss <- CVS.loadFromFile mibPath >>= liftEither
        cvps <- CVP.loadFromFile mibPath >>= liftEither

        tcds <- TCD.loadFromFile mibPath >>= liftEither
        scos <- SCO.loadFromFile mibPath >>= liftEither

        let (tcErrs, hm) = convertTCDef epoch
                                        coeff
                                        defaultConnName
                                        parHM
                                        (getCDFMap cdfs)
                                        (getCVSMap cvss)
                                        (getCVPMap cvps)
                                        (getTCDMap tcds)
                                        (getSCOMap scos)
                                        ccfs

            prfHM            = getPRFMap prfMap
            prvHM            = getPRVMap prvMap

            (parErrs, parHM) = convertTCParams epoch
                                               coeff
                                               prfHM
                                               prvHM
                                               numCalibs
                                               textCalibs
                                               cpcs

        return (nErrs ++ tErrs ++ tcErrs ++ parErrs, hm, parHM)
