module Main
where


import Data.PUS.Config


main :: IO ()
main = do
    --writeConfigString defaultConfig "DefaultConfig.config"
    writeConfigJSON defaultConfig "DefaultConfig.json"

    