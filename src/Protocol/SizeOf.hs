module Protocol.SizeOf
where
    

class SizeOf a where
    sizeof :: a -> Int
    sizeofBits :: a -> Int
    sizeofBits x = 8 * sizeof x
    
