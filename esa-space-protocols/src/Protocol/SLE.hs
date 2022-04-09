{-# LANGUAGE CPP #-}
module Protocol.SLE
    (
      startSLE 
    ) where


#ifdef HAS_SLE
import Protocol.Internal.SLE 
#else 
import Protocol.Internal.SLEDummy
#endif 
