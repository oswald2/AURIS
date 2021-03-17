module Data.Conversion.Info
    ( convertInfo
    ) where

--import           RIO


import           Data.PUS.DataModelInfo
import           Data.MIB.VDF
import           Data.MIB.Types

convertInfo :: VDFentry -> DataModelInfo
convertInfo VDFentry {..} = DataModelInfo
    { _dmiName    = _vdfName
    , _dmiComment = _vdfComment
    , _dmiDomain  = _vdfDomainID
    , _dmiRelease = getDefaultInt _vdfRelease
    , _dmiIssue   = getDefaultInt _vdfIssue
    }
