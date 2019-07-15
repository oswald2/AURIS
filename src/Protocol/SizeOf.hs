{-# LANGUAGE AllowAmbiguousTypes
    , NoImplicitPrelude
#-}
module Protocol.SizeOf
where


import RIO

class SizeOf a where
    sizeof :: a -> Int
    sizeofBits :: a -> Int
    sizeofBits x = 8 * sizeof x


class FixedSize a where
    fixedSizeOf :: Int



class BitSize a where
    bitSize :: a -> Int
