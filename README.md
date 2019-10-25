# AURIS
A new, open source mission control system in Haskell

The repository has been changed to a big monorepo now. It contains the other libraries which were previously separate git repositories.

## Building 

Currently, building via stack is recommended. Cabal new-* commands should also work, but currently there is no cabal.project file available. 

There are 3 stack.yaml files provided:
 * stack.yaml: this is for developing with non-optimized code
 * stack_opt.yaml: this is for the optimized build (much slower)
 * stack_llvm:yaml: optimized build via LLVM (much much slower)
 
 ## Data Processing

The architecture of the data processing backend looks like this:

![](./architecture.svg)

The real code is a bit more involved and complicated and is constantly growing. All data processing is currently done in 'esa-space-protocols' which is the main library

## User Interface 

The libraries are designed so that they can be used in a variety of situations: client/server, standalone as one binary, use the libraries alone in other projects etc. 

A standalone executable with a GUI is called AURISi (for AURIS integrated) in the 'aurisi' directory. Currently all it does is connect to a simulator via NCTRS protocol and display incoming telemetry packets.

Once the internal models are implemented and working, this will be improved.

## Database 

There is a preliminary implementation of a DB backend for events (thanks to Paolo and Matthias), but it is currently not used in AURISi. There is also an implementation of storing TM Frames, but due to a bug in Selda it is not present in this repository, but in Paolo's git account and will be merged at a later point in time. 

The idea is to support at least 2 backends: sqlite for testing campaigns and Postgres for MCS/CCS activities.

## Satellite Information Base

Currently the MIB library is under work to load satellite information from a MIB in SCOS-2000 Format (Version 6.9). It may be, that a CDM library for EGS-CC based configurations will follow.

## Protocols

The currently working protocol is NCTRS. Since it is not that common anymore, other protocols will also be supported. For C&C and EDEN protocol the base modules are there, but are currently not handled.

Also thinking about adding SLE (would be necessary and probably be an interface to ESA's SLE library) and Cortex.


## Notes

 * The [hashtables](https://github.com/gregorycollins/hashtables) library has been forked and directly put into this mono-repo. What has been modified is that an immutable hash table type has been added (but only for the Basic ST hash table), which can be obtained by calling `unsafeFreeze`. This allows to pass the HashTable out of the ST monad, but also only read-only functions are allowed. Currently, only `ilookup`, `fold` and `toList` are implemented.
