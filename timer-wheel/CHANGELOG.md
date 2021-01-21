# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0] - 2020-06-18

### Added
- Add `with`
- Add support for GHC 8.8, GHC 8.10

### Changed
- Change type of `spokes` from `Natural` to `Int`
- Change order of delay and action arguments in `register`, `register_`, `recurring`, and `recurring_`
- Simplify `cancel` to return `True` at most once
- Throw an error if a negative delay is provided to `register`, `register_`, `recurring`, or `recurring_`
- Fix underflow bug that affected recurring timers

### Removed
- Remove `create`, `destroy`
- Remove `TimerWheelDied` exception. `with` now simply re-throws the exception that the timer wheel thread throws
- Remove `InvalidTimerWheelConfig` exception. `error` is used instead
- Remove support for GHC < 8.6

## [0.2.0.1] - 2019-05-19

### Changed
- Swap out `ghc-prim` and `primitive` for `vector`

## [0.2.0] - 2019-02-03

### Added
- Add `destroy` function, for reaping the background thread
- Add `recurring_` function

### Changed
- If the timer wheel reaper thread crashes, it will propagate the exception to
the thread that spawned it
- `new` may now throw `InvalidTimerWheelConfig`
- The cancel action returned by `register` is now memoized, which fixes a bug
involving trying to cancel a `recurring` timer twice. The second call used to
spin forever and peg a CPU
- Use `Config` type for creating a timer wheel
- Change argument order around
- Rename `new` to `create`
- Make recurring timers more accurate

## [0.1.0] - 2018-07-18

### Added
- Initial release
