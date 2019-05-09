# esa-space-protocols

A Haskell library for handling space protocols from the European Space Ageny (ESA). 

Currently under heavy construction and creation. 

## Current Features ##

 * CLTU encoding/decoding with TC randomization
 * TC Transfer Frame encoding/decoding
 
Current work focuses on the command link. Goal is to have a conduit based pipeline of command encoding flowing from a command request -> TC packet -> PUS packet -> segments -> COP-1 -> TC transfer frames -> CLTU -> Transfer Protocol. Parts of this are already present, but currently not going through. Some code skeletons are available to lay out the types used (e.g. a module defining the data types and then a encoder which contains a conduit to transform the data type to the next layer).

Current focus is on the COP-1 state machine and the NCTRS protocol to have a first indication of successful transmissions in interworking with spacecraft simulators. 
 
 ## Building ##
 
 ## Building with stack ##
 
 You can use `stack` to build the project. Be aware, that by default the `llvm` flag is set in the `stack.yaml`. If you prefer the native code generator, just comment it out.
 
 ## Building with Cabal ##
 
 Currently, `cabal new-build` is used. It accepts the following flags:
  * `-f llvm`: build via LLVM

There are also unit tests available (with `cabal new-configure --enable-tests`) and 
also benchmarks (with `cabal new-configure --enable-benchmarks`).

Documentation can be built as usual with `cabal new-haddock`, though it is currently only exhaustive for CLTUs, Randomizer and the Config.

### Build within docker ###

 Prepare a docker image with `make docker-build`. It sets up a devlopment user "dev"
 which has the same UID and GID as the user at the docker host.
 This directory is mounted in the home directory of dev, so build artifacts 
 are persistent, editable and removable afterwards from the outside.

 Start the docker container with `make docker-run`.
 If within AUK and internet connection is necessary (e.g. for cabal update),
 source etc/setProxy.sh.
 
## TODO ##

First, get all working.
 * Commanding independent of spacecraft definitions (MIB, CDM)
 * Telemetry independent of spacecraft definitions, so the basic layers
 * Probably another library for MIB handling will be needed. Also, a general internal model should be present so that alternative representations (e.g. EGS-CCs CDM) can be used too.
 * Adding the MIB/data model to be able to send specific commands and extract TM
 * Play around with parallel TM extraction and how to distribute parameters
 
That's enough for the next weeks.
