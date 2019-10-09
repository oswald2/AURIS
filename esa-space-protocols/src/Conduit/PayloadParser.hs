{-# LANGUAGE
    AutoDeriveTypeable
    , BangPatterns
    , BinaryLiterals
    , ConstraintKinds
    , DataKinds
    , DefaultSignatures
    , DeriveDataTypeable
    , DeriveFoldable
    , DeriveFunctor
    , DeriveGeneric
    , DeriveTraversable
    , DoAndIfThenElse
    , EmptyDataDecls
    , ExistentialQuantification
    , FlexibleContexts
    , FlexibleInstances
    , FunctionalDependencies
    , GADTs
    , GeneralizedNewtypeDeriving
    , InstanceSigs
    , KindSignatures
    , LambdaCase
    , MonadFailDesugaring
    , MultiParamTypeClasses
    , MultiWayIf
    , NamedFieldPuns
    , NoImplicitPrelude
    , OverloadedStrings
    , PartialTypeSignatures
    , PatternGuards
    , PolyKinds
    , RankNTypes
    , RecordWildCards
    , ScopedTypeVariables
    , StandaloneDeriving
    , TupleSections
    , TypeFamilies
    , TypeSynonymInstances
    , ViewPatterns
#-}
module Conduit.PayloadParser
  ( payloadParserC
  , GetPayload(..)
  )
where

import           RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T

import           Conduit
import           Data.Attoparsec.ByteString


class GetPayload a where
    getPayload :: a -> ByteString



payloadParserC :: (Monad m, GetPayload i) => Parser o -> ConduitT i (i, o) m (Either Text ())
payloadParserC parser = do 
    let go prev parser' = do
            x <- await
            case x of
                Nothing -> do
                    pure (Right ())
                Just i  -> do
                    let bs = prev <> getPayload i
                    case parser' bs of
                        Done rest !r -> do 
                            yield (i, r) 
                            go rest (parse parser)
                        Fail _ _ msg -> pure (Left (T.pack msg)) 
                        Partial cont -> go prev cont 
   
    go B.empty (parse parser)

