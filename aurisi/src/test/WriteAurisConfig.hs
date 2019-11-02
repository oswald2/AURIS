module Main
where


import AurisConfig


main :: IO ()
main = do
    --writeConfigString defaultConfig "DefaultConfig.config"
    writeConfigJSON defaultConfig "DefaultConfig.json"

    