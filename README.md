Multiple [GHC](http://haskell.org/ghc) Versions for [Travis-CI](https://travis-ci.org)
============================================================

The purpose of this document is to describe how to set up the [`.travis.yml` script](http://about.travis-ci.org/docs/user/build-configuration/) in order to build and test your [cabalized](http://www.haskell.org/cabal) Haskell package with multiple [GHC](http://haskell.org/ghc) configurations. 
At time of writing [Travis-CI](https://travis-ci.org/) has [support for building Haskell packages](http://about.travis-ci.org/docs/user/languages/haskell/) but only for a single GHC configuration (i.e. *Haskell Platform 2012.2.0.0 with GHC 7.4.1*). By following this guide, you can set up [Travis-CI](https://travis-ci.org/) jobs which have access to the following GHC versions (all compiled for *Ubuntu Linux 12.04 LTS 64-bit*):

 - GHC 6.12.3,
 - GHC 7.0.1, GHC 7.0.2, GHC 7.0.3, GHC 7.0.4,
 - GHC 7.2.1, GHC 7.2.2,
 - GHC 7.4.1, GHC 7.4.2,
 - GHC 7.6.1, GHC 7.6.2, GHC 7.6.3,
 - GHC 7.8.1, GHC 7.8.2, GHC 7.8.3, GHC 7.8.4
 - GHC 7.10.1 *(snapshots)*
 - GHC HEAD.

Each GHC version is provided in a separate `ghc-<version>` `.deb` package installing into `/opt/ghc/<version>` (thus allowing to be installed at the same time if needed) published in a [PPA](https://launchpad.net/~hvr/+archive/ghc). The easiest way to "activate" a particular GHC version is to prepend its `bin`-folder to the `$PATH` environment variable (see example in next section).

Note: For actually enabling continuous integration for a GitHub hosted project, see section [Getting Started](http://about.travis-ci.org/docs/user/getting-started/) in [Travis-CI](https://travis-ci.org/)'s online documentation.

### Add-on Packages

For convenience, a few add-on packages are available to provide more recent versions of `cabal`, `alex` and `happy` than are available in Ubuntu 12.04.

They install into a respective `/opt/<name>/<version>/bin` folder which can be put into the search `$PATH`.

| `.deb` Package Name  | Executable 
| -------------------- | ----------
| `cabal-install-1.16` | `/opt/cabal/1.16/bin/cabal`
| `cabal-install-1.18` | `/opt/cabal/1.18/bin/cabal`
| `cabal-install-1.20` | `/opt/cabal/1.20/bin/cabal`
| `cabal-install-1.22` | `/opt/cabal/1.22/bin/cabal`
| `cabal-install-head` | `/opt/cabal/head/bin/cabal`
| `alex-3.1.3`         | `/opt/alex/3.1.3/bin/alex`
| `happy-1.19.3`       | `/opt/happy/1.19.3/bin/happy`
| `happy-1.19.4`       | `/opt/happy/1.19.4/bin/happy`

See examples below for how to use those.

`.travis.yml` Template
----------------------

Below is a commented `.travis.yml` example that can be used as a template:

```yaml
# NB: don't set `language: haskell` here

# The following enables several GHC versions to be tested; often it's enough to test only against the last release in a major GHC version. Feel free to omit lines listings versions you don't need/want testing for.
env:
 - CABALVER=1.16 GHCVER=6.12.3
 - CABALVER=1.16 GHCVER=7.0.1
 - CABALVER=1.16 GHCVER=7.0.2
 - CABALVER=1.16 GHCVER=7.0.3
 - CABALVER=1.16 GHCVER=7.0.4
 - CABALVER=1.16 GHCVER=7.2.1
 - CABALVER=1.16 GHCVER=7.2.2
 - CABALVER=1.16 GHCVER=7.4.1
 - CABALVER=1.16 GHCVER=7.4.2
 - CABALVER=1.16 GHCVER=7.6.1
 - CABALVER=1.16 GHCVER=7.6.2
 - CABALVER=1.18 GHCVER=7.6.3
 - CABALVER=1.18 GHCVER=7.8.1  # see note about Alex/Happy for GHC >= 7.8
 - CABALVER=1.18 GHCVER=7.8.2
 - CABALVER=1.18 GHCVER=7.8.3
 - CABALVER=1.22 GHCVER=7.10.1
 - CABALVER=head GHCVER=head   # see section about GHC HEAD snapshots

# Note: the distinction between `before_install` and `install` is not important.
before_install:
 - travis_retry sudo add-apt-repository -y ppa:hvr/ghc
 - travis_retry sudo apt-get update
 - travis_retry sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER # see note about happy/alex
 - export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

install:
 - cabal --version
 - echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
 - travis_retry cabal update
 - cabal install --only-dependencies --enable-tests --enable-benchmarks

# Here starts the actual work to be performed for the package under test; any command which exits with a non-zero exit code causes the build to fail.
script:
 - if [ -f configure.ac ]; then autoreconf -i; fi
 - cabal configure --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
 - cabal build   # this builds all libraries and executables (including tests/benchmarks)
 - cabal test
 - cabal check
 - cabal sdist   # tests that a source-distribution can be generated

# The following scriptlet checks that the resulting source distribution can be built & installed
 - export SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
   cd dist/;
   if [ -f "$SRC_TGZ" ]; then
      cabal install --force-reinstalls "$SRC_TGZ";
   else
      echo "expected '$SRC_TGZ' not found";
      exit 1;
   fi

```

For more information about the `.travis.yml` script please consult the
[official documentation](http://about.travis-ci.org/docs/user/build-configuration/).

### Haskell Platform Configurations

Basic idea: Generate a `cabal.config` file during the build job (before installing the build-dependencies) constraining to HP package versions, e.g. for HP 2013.2.0.0 the `cabal.config` would need to contain the following constraints definition:

```
constraints: async==2.0.1.4,attoparsec==0.10.4.0,case-insensitive==1.0.0.1,cgi==3001.1.7.5,fgl==5.4.2.4,GLUT==2.4.0.0,GLURaw==1.3.0.0,haskell-src==1.0.1.5,hashable==1.1.2.5,html==1.0.1.2,HTTP==4000.2.8,HUnit==1.2.5.2,mtl==2.1.2,network==2.4.1.2,OpenGL==2.8.0.0,OpenGLRaw==1.3.0.0,parallel==3.2.0.3,parsec==3.1.3,QuickCheck==2.6,random==1.0.1.1,regex-base==0.93.2,regex-compat==0.95.1,regex-posix==0.95.2,split==0.2.2,stm==2.4.2,syb==0.4.0,text==0.11.3.1,transformers==0.3.0.0,unordered-containers==0.2.3.0,vector==0.10.0.1,xhtml==3000.2.1,zlib==0.5.4.1
```

Use [this `.travis.yml` script](.travis.yml) as a template if you want
to test against Haskell Platform configurations.

### Alex & Happy with GHC ≥ 7.8

If your package (or one of its dependencies) contain Alex/Happy generated parsers, GHC 7.8.1 and later require a more recent `alex`/`happy` executable installed (see [Happy, Alex, and GHC 7.8](http://ro-che.info/articles/2014-03-08-happy-alex-ghc-7.8.html) for the gory details). The following snipped (stolen from `lens`'s [`.travis.yaml`](https://github.com/ekmett/lens/blob/master/.travis.yml)) illustrates how to this can be accomplished:

```yaml
 - |
   if [ $GHCVER = "head" ] || [ ${GHCVER%.*} = "7.8" ] || [ ${GHCVER%.*} = "7.10" ]; then
     travis_retry sudo apt-get install happy-1.19.4 alex-3.1.3
     export PATH=/opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:$PATH
   else
     travis_retry sudo apt-get install happy alex
   fi
```

### GHC HEAD Snapshots

 - Snapshots of current GHC development snapshots from the `master` branch (aka *GHC HEAD*) are uploaded at irregular intervals to the PPA
 - You can select *GHC HEAD* at your own risk by setting `GHCVER=head`
 - As GHC HEAD is experimental and likely to cause build failures, you might want to [tolerate failures](http://about.travis-ci.org/docs/user/build-configuration/#Rows-That-are-Allowed-To-Fail) by adding the following snippet to your `.travis.yml`:

    ```yaml
    matrix:
      allow_failures:
       - env: CABALVER=head GHCVER=head
    ```

 - NB: the line in `matrix.allow_failures.env` must match exactly
   (including any whitespace) the line specified in `env`

Ideas for Additional Checks
---------------------------

 - Check for code-smell via [`hlint`](http://hackage.haskell.org/package/hlint)
 - Check for `build-depends` excluding latest package versions with [`packdeps`](http://hackage.haskell.org/package/packdeps)
 - Check for unusued `build-depends` with [`packunused`](http://hackage.haskell.org/package/packunused)
 - Check for 100% Haddock coverage
 - Check for trailing whitespaces and/or tabs in source files

Random Remarks
--------------

 - If you want to know which core library version each GHC used (e.g. for deciding on what upper/lower bounds to declare for `build-depends`), see [GHC Boot Library Version History](http://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory)
 - Supporting GHC versions prior to 7.0.1 requires more effort:
    - GHC 7.0.1 was the first version to support `default-language: Haskell2010`
    - Declaring `cabal-version >= 1.10` makes it more difficult to compile with GHC 6.12.3's default `cabal-install`
    - `cabal-install` [falls back to top-down solver for GHC < 7](http://stackoverflow.com/questions/16021645/what-does-cabals-warning-falling-back-to-topdown-solver-for-ghc-7-mean) which may require additional tweaks to the build script to compensate for (e.g. installing `QuickCheck` via `cabal install --only-dep` is known to fail)

Real-world Examples
-------------------

 - [lens](https://github.com/ekmett/lens) [![Build Status](https://travis-ci.org/ekmett/lens.png?branch=master)](https://travis-ci.org/ekmett/lens)
 - [bytestring](https://github.com/haskell/bytestring) [![Build Status](https://travis-ci.org/haskell/bytestring.png?branch=master)](https://travis-ci.org/haskell/bytestring)
 - [Cabal](https://github.com/haskell/cabal) [![Build Status](https://travis-ci.org/haskell/cabal.png?branch=master)](https://travis-ci.org/haskell/cabal)
 - [deepseq-generics](https://github.com/hvr/deepseq-generics) [![Build Status](https://travis-ci.org/hvr/deepseq-generics.png?branch=master)](https://travis-ci.org/hvr/deepseq-generics)
 - [filepath](https://github.com/ghc/packages-filepath)  [![Build Status](https://travis-ci.org/ghc/packages-filepath.png)](https://travis-ci.org/ghc/packages-filepath)
 - [arbtt](https://github.com/nomeata/darcs-mirror-arbtt)  [![Build Status](https://travis-ci.org/nomeata/darcs-mirror-arbtt.png)](https://travis-ci.org/nomeata/darcs-mirror-arbtt)
 - [circle-packing](https://github.com/nomeata/darcs-mirror-circle-packing)  [![Build Status](https://travis-ci.org/nomeata/darcs-mirror-circle-packing.png)](https://travis-ci.org/nomeata/darcs-mirror-circle-packing)
