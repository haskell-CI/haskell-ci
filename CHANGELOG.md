## 0.8

- Split parsing utilities into [cabal-install-parsers package](https://hackage.haskell.org/package/cabal-install-parsers)
- `--hlint-download-binary` to download HLint instead of building it ourselves [#323](https://github.com/haskell-ci/haskell-ci/pull/323)
- Fix `haddock` path in OSX builds [#318](https://github.com/haskell-ci/haskell-ci/pull/318)
- Don't `brew upgrade` on OSX, which greatly speedups builds [#320](https://github.com/haskell-ci/haskell-ci/pull/320)
- Use Travis built-in config validation [#338](https://github.com/haskell-CI/haskell-ci/pull/338)

## 0.6

- ghc-options limiting heap size
- GHCJS tests can be run in simple cases
- Work around cabal#6214 (haddock failing with `build-type: Custom` packages)
- Support requesting `google-chrome` addon
- Nicer `docctest-filter-packages`
- Buildable with GHC-8.8
- Record `haskell-ci` version in REGENDATA;
  warn if older executable is used to `regenerate`

## 0.4

* Make default `--output` to be `.travis.yml`; use `--stdout` to output to standard output.
* Add GHC-8.8 support
* Use cabal-install-3.0 by default
* Experimental support for GHCJS jobs
* A lot of new configuration options

## 0.2.1

* `local-ghc-options` are always applied (independent of `copy-fields` value)
* Add GHC-8.6.4 to known compilers

## 0.2

* Large configuration rework
