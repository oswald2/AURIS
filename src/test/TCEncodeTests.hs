{-# LANGUAGE 
    BangPatterns
    , OverloadedStrings
    , NoImplicitPrelude
#-}
module Main 
where


import RIO

import Data.PUS.Parameter
import Data.PUS.Types
import Data.PUS.Value

import General.Types

import System.IO

parameters :: ParameterList
parameters = List [Parameter "P1" (ValInt16 BiE 0xaabb)] 
                    (Group (Parameter "N" (ValInt8 3)) (
                        List [Parameter "G1" (ValUInt3 1), Parameter "G2" (ValDouble BiE 3.14)] Empty
                    ))
                



main :: IO ()
main = do 

    let lst = expandGroups parameters

    putStrLn $ "Parameters:\n" <> show parameters
    putStrLn $ "\nExpanded:\n" <> show lst

    return ()