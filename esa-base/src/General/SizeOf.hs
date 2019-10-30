{-# LANGUAGE AllowAmbiguousTypes
    , NoImplicitPrelude
#-}
module General.SizeOf
    (
        SizeOf(..)
        , FixedSize(..)
        , BitSizes(..)
    )
where


import           RIO

import           General.Types


class SizeOf a where
    sizeof :: a -> Int
    sizeofBits :: a -> Int
    sizeofBits x = 8 * sizeof x


class FixedSize a where
    fixedSizeOf :: Int



class BitSizes a where
    bitSize :: a -> BitSize
