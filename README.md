Haskell-CI: Continuous Integration for Haskell [![Hackage](https://img.shields.io/hackage/v/haskell-ci.svg)](https://hackage.haskell.org/package/haskell-ci)[![Travis Build](https://img.shields.io/travis/haskell-CI/haskell-ci.svg)](https://travis-ci.org/haskell-CI/haskell-ci)[![Build Status](https://github.com/haskell-CI/haskell-ci/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-CI/haskell-ci/actions?query=workflow%3Ahaskell-ci)
============================================================

This project provides a script generator for continuous-integration testing of Haskell packages. It currently provides substantive support for:

- [Travis-CI](https://travis-ci.org/)
- [GitHub Actions](https://docs.github.com/en/actions)

Features include:

- Multiple [GHC](http://haskell.org/ghc) support
- Building with specific constraints
- Dependency caching
- cabal and cabal.project support (see [Nix-style local builds documentation](https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html))
- Runs tests and builds benchmarks
- Support for Haskell build tools, including [Haddock](https://hackage.haskell.org/package/haddock), [GHCJS](https://github.com/ghcjs/ghcjs) & [HLint](https://hackage.haskell.org/package/hlint)
- doctest runner integration using either [doctest](https://hackage.haskell.org/package/doctest) or [cabal-docspec](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md).

Usage
---

See `haskell-ci --help` for details.

1. Install `haskell-ci`

```bash
cabal install haskell-ci
```

2. Add a `tested-with` line to your .cabal file (e.g. `tested-with: GHC == 8.10.4 || == 9.0.1`)

3. Create a configuration file. A good starting point for this is the `dump-config` command eg

``` shell
haskell-ci dump-config > haskell-ci.config
```

4. Run the `github` or `travis` commands as appropriate to your workflow. eg

``` shell
haskell-ci github '--config=haskell-ci.config' 'cabal.project' --output .github/workflows/haskell-ci.yml
```

``` shell
haskell-ci travis '--config=haskell-ci.config' 'cabal.project' --output .travis.yml
```

5. Push your project adding the files created, which should trigger the appropriate CI for your setup.

GitHub actions example:

    ```bash
    $ git checkout master            # Check out `master`
    $ git pull --ff-only             # Get the latest version of `master`
    $ git checkout -b new_action     # Create a `new_action` branch
    $ git add .github/workflows/haskell-ci.yml
    $ git commit -m "New action"
    $ git push -u origin new_action  # Push your branch upstream
    ```

GitHub actions will automatically be triggered.

Travis-CI example:

    ```bash
    $ git checkout master            # Check out `master`
    $ git pull --ff-only             # Get the latest version of `master`
    $ git checkout -b new_travis     # Create a `new_travis` branch
    $ git add .travis.yml
    $ git commit -m "New Travis script"
    $ git push -u origin new_travis  # Push your branch upstream
    ```
    
    If you have Travis enabled for your repository this will test your branch
    using your newly created Travis file.  This lets you test the Travis script
    before merging the new script into `master`.

6. Fix the build

    If you're lucky, your repository will build for every compiler version
    you listed.  If that's the case, then just merge your changes into `master`:
    
    ```bash
    $ git checkout master
    $ git merge new_action  # Update `master` with your new script
    $ git push
    ```
    
    You can also merge your branch into `master` from GitHub's pull request view.
    
    If you're not lucky, then your new branch will fail for one or more
    versions of GHC, which is okay!  Look at the build and fix any build failures
    you find and commit the fixes to your branch:
    
    ```bash
    $ # Fix any build failures you find and commit your changes
    $ ...
    $ git push  # Push your branch updates upstream
    ```
    
    Sometimes you may need to regenerate Travis script. for example, to
    remove the `cabal check` step (pass `--no-cabal-check` flag to `haskell-ci)
    if you know for sure that you need build your project with the `-O2` flag.
    
    Each time you push an update to your branch Travis will run again to see if
    any build failures still remain.  Repeat this process until your project
    builds against each GHC version you listed.  Once your project builds against
    each target version of GHC you can merge your script into `master`

7. Add a badge.

To add a badge to your project readme, signalling build status, follow these templates:

For Travis-CI:

``` bash
[![Travis Build](https://img.shields.io/travis/<your name>/<repo name>.svg)](https://travis-ci.org/<your name>/<repo name>)
```

For a GitHub action:

``` bash
[![Build Status](https://github.com/<your name>/<repo name>/workflows/<github action name>/badge.svg)](https://github.com/<your name>/<repo name>/actions?query=workflow%3A<github action name>)
```

Support & Development
---

The intention of the package is to support modern continuous integration for Haskell projects. Given this intent, and given the nature of rapid development of GHC and Haskell tooling, this project requires active maintenance and development. 

There are likely to be delays between support for GHC versions and new project releases. Users who are developing and tracking close to latest release should install `haskell-ci` directly from the repository; eg

``` bash
git clone https://github.com/haskell-CI/haskell-ci.git
cd haskell-ci
cabal install haskell-ci:exe:haskell-ci
```

Given the evolving nature of the Haskell tool-set, options and configuration will continue to undergo transformation. New CI practices will cause change, some existing functionality may cease to be supported, or become non-standard practice. 

At time of writing (August 2021):

- GitHub action support continues active development, and Travis-CI support is being maintained.
- `ghcup` is starting to be used to help configure builds, replacing the current reliance on PPA.
- macos support has been recently introduced and is in a testing phase.
- GHC snapshots are no longer uploaded to PPA, and the `ghc-head` configuration option is thus unsupported. `ghcup` can be used to test prereleases by adding the recent version to the `tested-with`. For example, at time of writing `tested-with: GHC ==8.10.4 || ==9.2.0.20210821` creates a test that includes the 9.2 RC3 release.

Ideas for Additional Checks
---------------------------

 - Check for `build-depends` excluding latest package versions with [`packdeps`](http://hackage.haskell.org/package/packdeps)
 - Check for unused `build-depends` with [`packunused`](http://hackage.haskell.org/package/packunused)
 - Check for 100% Haddock coverage
 - Check for trailing whitespaces and/or tabs in source files
 - Support for [weeder](https://hackage.haskell.org/package/weeder), [stan](https://github.com/kowainik/stan#stan) and other tools as they gain wider usage.
 - Support for [stack](https://docs.haskellstack.org/en/stable/README/) and windows environment building.
 - Style and format checking such as [ormolu](https://hackage.haskell.org/package/ormolu)

Real-world Examples
-------------------

To understand `haskell-ci`, and why you would want to use it to generate scripts for GitHub actions, or if you want to step outside the restricted confine of automation, users should familiarise themselves with the [haskell/actions repository](https://github.com/haskell/actions/tree/main/setup#readme).

Some projects that currently use `haskell-ci` include:

- [lens](https://github.com/ekmett/lens) [![Build Status](https://github.com/ekmett/lens/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/lens/actions?query=workflow%3Ahaskell-ci)
- [generics-sop](https://github.com/well-typed/generics-sop) [![Build Status](https://github.com/well-typed/generics-sop/workflows/Haskell-CI/badge.svg)](https://github.com/well-typed/generics-sop/actions?query=workflow%3Ahaskell-ci)
- [input-parsers](https://github.com/blamario/input-parsers) [![Build Status](https://github.com/blamario/input-parsers/workflows/Haskell-CI/badge.svg)](https://github.com/blamario/input-parsers/actions?query=workflow%3Ahaskell-ci)
- [generic-deriving](https://github.com/dreixel/generic-deriving) [![Build Status](https://github.com/dreixel/generic-deriving/workflows/Haskell-CI/badge.svg)](https://github.com/dreixel/generic-deriving/actions?query=workflow%3Ahaskell-ci)
- [smash](https://github.com/emilypi/smash) [![Build Status](https://github.com/emilypi/smash/workflows/Haskell-CI/badge.svg)](https://github.com/emilypi/smash/actions?query=workflow%3Ahaskell-ci)
- [scientific](https://github.com/basvandijk/scientific) [![Build Status](https://github.com/basvandijk/scientific/workflows/Haskell-CI/badge.svg)](https://github.com/basvandijk/scientific/actions?query=workflow%3Ahaskell-ci)
- [numhask](https://github.com/tonyday567/numhask) [![Build Status](https://github.com/tonyday567/numhask/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/numhask/actions?query=workflow%3Ahaskell-ci)
- [tasty-bench](https://github.com/Bodigrim/tasty-bench) [![Build Status](https://github.com/Bodigrim/tasty-bench/workflows/Haskell-CI/badge.svg)](https://github.com/Bodigrim/tasty-bench/actions?query=workflow%3Ahaskell-ci)
- [agda-stdlib](https://github.com/agda/agda-stdlib) [![Build Status](https://github.com/agda/agda-stdlib/workflows/Haskell-CI/badge.svg)](https://github.com/agda/agda-stdlib/actions?query=workflow%3Ahaskell-ci)
- [broadcast-chan](https://github.com/merijn/broadcast-chan) [![Build Status](https://github.com/merijn/broadcast-chan/workflows/Haskell-CI/badge.svg)](https://github.com/merijn/broadcast-chan/actions?query=workflow%3Ahaskell-ci)
- [checkers](https://github.com/haskell-checkers/checkers) [![Build Status](https://github.com/haskell-checkers/checkers/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-checkers/checkers/actions?query=workflow%3Ahaskell-ci)
- [text-show](https://github.com/RyanGlScott/text-show) [![Build Status](https://github.com/RyanGlScott/text-show/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/text-show/actions?query=workflow%3Ahaskell-ci)
- [postgresql-simple](https://github.com/haskellari/postgresql-simple) [![Build Status](https://github.com/haskellari/postgresql-simple/workflows/Haskell-CI/badge.svg)](https://github.com/haskellari/postgresql-simple/actions?query=workflow%3Ahaskell-ci)
- [optparse-applicative](https://github.com/pcapriotti/optparse-applicative) [![Build Status](https://github.com/pcapriotti/optparse-applicative/workflows/Haskell-CI/badge.svg)](https://github.com/pcapriotti/optparse-applicative/actions?query=workflow%3Ahaskell-ci)
- [natural-transformation](https://github.com/ku-fpg/natural-transformation) [![Build Status](https://github.com/ku-fpg/natural-transformation/workflows/Haskell-CI/badge.svg)](https://github.com/ku-fpg/natural-transformation/actions?query=workflow%3Ahaskell-ci)


