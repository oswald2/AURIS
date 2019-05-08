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

### Build within docker ###

 Prepare a docker image with make `docker-build`. It sets up a devlopment user "dev"
 which has the same UID and GID as the user at the docker host.
 This directory is mounted in the home directory of dev, so build artifacts 
 are persistent, editable and removable afterwards from the outside.

 Start the docker container with `make docker-run`.
 If within AUK and internet connection is necessary (e.g. for cabal update),
 source etc/setProxy.sh.
 