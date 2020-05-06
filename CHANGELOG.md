## 0.10.1 - 20020-05-16

- Add version-info command to help debugging issues with `haskell-ci`
- Bump default HLint version to 3.1

## 0.10 - 2020-04-14

- Allow turning off email notifications
- Use generic-lens-lite instead of generic-lens (smaller dependency footprint)
- Add GHC-8.10, use cabal-install-3.2
- Fix escaping of irc-notification template
- Remove output colouring, it was relying on Cabal's internal features
- Add `-Werror=missing-methods` by default.

## 0.8 - 2019-11-26

- Split parsing utilities into [cabal-install-parsers package](https://hackage.haskell.org/package/cabal-install-parsers)
- `--hlint-download-binary` to download HLint instead of building it ourselves [#323](https://github.com/haskell-ci/haskell-ci/pull/323)
- Fix `haddock` path in OSX builds [#318](https://github.com/haskell-ci/haskell-ci/pull/318)
- Don't `brew upgrade` on OSX, which greatly speedups builds [#320](https://github.com/haskell-ci/haskell-ci/pull/320)
- Use Travis built-in config validation [#338](https://github.com/haskell-CI/haskell-ci/pull/338)
- Use `sourceline: ...` explicit entry for hvr-ppa unconditionally [#338](https://github.com/haskell-CI/haskell-ci/pull/338)

## 0.6 - 2019-10-21

- ghc-options limiting heap size
- GHCJS tests can be run in simple cases
- Work around cabal#6214 (haddock failing with `build-type: Custom` packages)
- Support requesting `google-chrome` addon
- Nicer `docctest-filter-packages`
- Buildable with GHC-8.8
- Record `haskell-ci` version in REGENDATA;
  warn if older executable is used to `regenerate`

## 0.4 - 2019-08-28

* Make default `--output` to be `.travis.yml`; use `--stdout` to output to standard output.
* Add GHC-8.8 support
* Use cabal-install-3.0 by default
* Experimental support for GHCJS jobs
* A lot of new configuration options

## 0.2.1 - 2019-03-06

* `local-ghc-options` are always applied (independent of `copy-fields` value)
* Add GHC-8.6.4 to known compilers

## 0.2 - 2019-03-03

* Large configuration rework
