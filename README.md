# esa-space-protocols

A Haskell library for handling space protocols from the European Space Ageny (ESA). 

Currently under heavy construction and creation. 

## Current Features ##

 * CLTU encoding/decoding with TC randomization
 * TC Transfer Frame encoding/decoding
 
 
 ## Building ##
 
 Currently, `cabal new-build` is used. It accepts the following flags:
  * `-f llvm`: build via LLVM

There are also unit tests available (with `cabal new-configure --enable-tests`) and 
also benchmarks (with `cabal new-configure --enable-benchmarks`).

