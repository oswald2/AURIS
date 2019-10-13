module Main where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                   as V

import qualified Data.Text.IO                  as T
import           UnliftIO.Environment

import           Data.MIB.LoadMIB
import           Data.MIB.CAF                  as CAF
import           Data.MIB.CAP                  as CAP
import           Data.MIB.MCF                  as MCF
import           Data.MIB.LGF                  as LGF
import           Data.MIB.TXF                  as TXF
import           Data.MIB.TXP                  as TXP
import           Data.MIB.CUR                  as CUR 
import           Data.MIB.PCF                  as PCF


import           Test.Hspec
import           Text.Show.Pretty



newtype TestState = TestState { logFunction :: LogFunc}

instance HasLogFunc TestState where
    logFuncL = lens logFunction (\c lf -> c { logFunction = lf })


testCaf :: FilePath -> IO ()
testCaf = testLoadTab CAF.loadFromFile

testCap :: FilePath -> IO ()
testCap = testLoadTab CAP.loadFromFile

testMcf :: FilePath -> IO ()
testMcf = testLoadTab MCF.loadFromFile

testLgf :: FilePath -> IO ()
testLgf = testLoadTab LGF.loadFromFile

testTxp :: FilePath -> IO ()
testTxp = testLoadTab TXP.loadFromFile

testTxf :: FilePath -> IO ()
testTxf = testLoadTab TXF.loadFromFile

testCur :: FilePath -> IO ()
testCur = testLoadTab CUR.loadFromFile

testPcf :: FilePath -> IO ()
testPcf = testLoadTab PCF.loadFromFile


testLoadTab :: Show b => (FilePath -> RIO TestState (Either Text (Vector b))) -> FilePath-> IO ()
testLoadTab action mibPath = do
    elems <- runRIOTestAction (action mibPath)
    case elems of
        Left  err -> do 
            T.putStrLn err
            exitFailure
        Right c   -> do
            pPrint c
            T.putStrLn $ "Count: " <> T.pack (show (V.length c))


runRIOTestAction :: RIO TestState b -> IO b
runRIOTestAction action = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelError defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        let state = TestState logFunc

        runRIO state action


testLoadCalibs :: FilePath -> IO () 
testLoadCalibs mibPath = do 
    res <- runRIOTestAction (loadCalibs mibPath)
    case res of 
        Left err -> do 
            T.putStrLn err 
            exitFailure 
        Right r -> pPrint r



main :: IO ()
main = do
    [mibPath] <- getArgs

    -- T.putStrLn "Loading Tables:\n===============\n"
    -- T.putStrLn "CAFs:\n"
    -- testCaf mibPath
    -- T.putStrLn "\n\n\nCAPs:\n"
    -- testCap mibPath
    -- T.putStrLn "\n\n\nMCFs:\n"
    -- testMcf mibPath
    -- T.putStrLn "\n\n\nLGFs:\n"
    -- testLgf mibPath
    -- T.putStrLn "\n\n\nTXFs:\n"
    -- testTxf mibPath
    -- T.putStrLn "\n\n\nTXPs:\n"
    -- testTxp mibPath
    -- T.putStrLn "\n\n\nCURs:\n"
    -- testCur mibPath
    -- T.putStrLn "\n\n\nPCFs:\n"
    -- testPcf mibPath

    T.putStrLn "\n\n\nLoading Data Structures:\n===============\n"
    T.putStrLn "LoadCalibs:\n"
    testLoadCalibs mibPath 

