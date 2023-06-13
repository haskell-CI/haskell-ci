## 0.16.5 - 2023-06-13

- Use optparse-applicative-0.18.1.
  When using older optparse-applicative with prettyprinter,
  `--help` appeared to hang.
- Update other dependencies as well.

## 0.16.4 - 2023-06-09

- Add GHC-9.2.8

## 0.16.3 - 2023-05-25

- Add GHC-9.6.2

## 0.16.2 - 2023-05-17

- Add GHC-9.4.5
- Use cabal-docspec-20230517

## 0.16 - 2023-04-21

- Add GHC-9.4.5

## 0.16 - 2023-04-06

- Add compiler up to GHC-9.6.1
- Update tools (hlint, cabal-docspec, cabal-plan, doctest, cabal-install)
- Use ShellCheck-0.9.0
- Use postgresql 14
- head.hackage overrides hackage
- Update GHA actions
- Save GHA cache in failing jobs also
- Add `-Werror=missing-fields` in addition to existing `-Werror=missing-methods`

## 0.14.3 - 2022-03-07

- Add GHC-9.2.2
- Use ghcup-0.1.17.5

## 0.14.2 - 2022-02-19

- Pass `default-language` to `doctest`
- Support local `.tar.gz` tarballs in `packages:`.

## 0.14 - 2022-01-05

- Add GHC-9.0 releases and GHC-9.2.1
- Add ghcup setup method. It is used by default for GHC versions not in hvr-ppa.
- GHA: Specify GHC memory limit
- GHA: timeout-minutes configuration option
- Update cabal-docspec versions used

## 0.12.1 - 2021-03-20

- Use HLint-3.3
- GHA: Support --distribution and --submodules (thanks to Ryan Scott)

## 0.12 - 2021-03-13

- Use `cabal-install-3.4`
- Add `bash` (Bash + Docker) generator
- Add `github` (GitHub Actions) generator

## 0.10.3 - 2020-08-10

- Add GHC-8.8.4 and GHC-8.10.2

## 0.10.2 - 2020-06-05

- Bump default doctest version to 0.17
- Fix HCPKG variable in macOS builds [#395](https://github.com/haskell-CI/haskell-ci/issues/395)

## 0.10.1 - 2020-05-16

- Add version-info command to help debugging issues with `haskell-ci`
- Bump default HLint version to 3.1
- Support ShellCheck with GHC-8.8 and GHC-8.10

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
