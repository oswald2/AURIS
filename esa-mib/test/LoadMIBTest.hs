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


testLoadTab :: Show b => (FilePath -> RIO TestState (Either Text (Vector b))) -> FilePath-> IO ()
testLoadTab action mibPath = do
    elems <- runRIOTestAction (action mibPath)
    case elems of
        Left  err -> T.putStrLn err
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




main :: IO ()
main = do
    [mibPath] <- getArgs

    T.putStrLn "Loading Tables:\n===============\n"
    T.putStrLn "CAFs:\n"
    testCaf mibPath
    T.putStrLn "\n\n\nCAPs:\n"
    testCap mibPath
    T.putStrLn "\n\n\nMCFs:\n"
    testMcf mibPath
    T.putStrLn "\n\n\nLGFs:\n"
    testLgf mibPath

