{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Version
    (
        aurisVersion
        ,aurisVersionString
    )
where


import Data.Text as T
import Development.GitRev


aurisVersion :: Text
aurisVersion = T.concat ["Version: 0.1.0.0 ", "Branch: ", $(gitBranch), " ", $(gitHash), "\ndirty: ", dirty, "\nCommit Date: ", $(gitCommitDate) ]

dirty :: Text
dirty | $(gitDirty) = "true"
      | otherwise = "false"

aurisVersionString :: String
aurisVersionString = T.unpack aurisVersion
