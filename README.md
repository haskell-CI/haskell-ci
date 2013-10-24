Multiple [GHC](http://haskell.org/ghc) Versions for [Travis-CI](https://travis-ci.org)
============================================================

The purpose of this document is to describe how to set up the [`.travis.yml` script](http://about.travis-ci.org/docs/user/build-configuration/) in order to build and test your [cabalized](http://www.haskell.org/cabal) Haskell package with multiple [GHC](http://haskell.org/ghc) configurations. 
At time of writing [Travis-CI](https://travis-ci.org/) has [support for building Haskell packages](http://about.travis-ci.org/docs/user/languages/haskell/) but only for a single GHC configuration (i.e. *Haskell Platform 2012.2.0.0 with GHC 7.4.1*). By following this guide, you can set up [Travis-CI](https://travis-ci.org/) jobs which have access to the following GHC versions (all compiled for *Ubuntu Linux 12.04 LTS 64-bit*):

 - GHC 6.12.3,
 - GHC 7.0.1, GHC 7.0.2, GHC 7.0.3, GHC 7.0.4,
 - GHC 7.2.1, GHC 7.2.2,
 - GHC 7.4.1, GHC 7.4.2,
 - GHC 7.6.1, GHC 7.6.2, GHC 7.6.3,
 - GHC HEAD.

Each GHC version is provided in a separate `ghc-<version>` `.deb` package installing into `/opt/ghc/<version>` (thus allowing to be installed at the same time if needed) published in a [PPA](https://launchpad.net/~hvr/+archive/ghc). The easiest way to "activate" a particular GHC version is to prepend its `bin`-folder to the `$PATH` environment variable (see example in next section).

Moreover, a `cabal-install-1.18` `.deb` package providing the `/usr/bin/cabal-1.18` executable for the current stable `1.18` series is also available for convenience.

Note: For actually enabling continuous integration for a GitHub hosted project, see section [Getting Started](http://about.travis-ci.org/docs/user/getting-started/) in [Travis-CI](https://travis-ci.org/)'s online documentation.

`.travis.yml` Template
----------------------

Below is a commented `.travis.yml` example that can be used as a template:

```yaml
# NB: don't set `language: haskell` here

# The following enables several GHC versions to be tested; often it's enough to test only against the last release in a major GHC version. Feel free to omit lines listings versions you don't need/want testing for.
env:
 - GHCVER=6.12.3
 - GHCVER=7.0.1
 - GHCVER=7.0.2
 - GHCVER=7.0.3
 - GHCVER=7.0.4
 - GHCVER=7.2.1
 - GHCVER=7.2.2
 - GHCVER=7.4.1
 - GHCVER=7.4.2
 - GHCVER=7.6.1
 - GHCVER=7.6.2
 - GHCVER=7.6.3
# - GHCVER=head  # see section about GHC HEAD snapshots

# Note: the distinction between `before_install` and `install` is not important.
before_install:
 - sudo add-apt-repository -y ppa:hvr/ghc
 - sudo apt-get update
 - sudo apt-get install cabal-install-1.18 ghc-$GHCVER happy
 - export PATH=/opt/ghc/$GHCVER/bin:$PATH

install:
 - cabal-1.18 update
 - cabal-1.18 install --only-dependencies --enable-tests --enable-benchmarks

# Here starts the actual work to be performed for the package under test; any command which exits with a non-zero exit code causes the build to fail.
script:
 - cabal-1.18 configure --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
 - cabal-1.18 build   # this builds all libraries and executables (including tests/benchmarks)
 - cabal-1.18 test
 - cabal-1.18 check
 - cabal-1.18 sdist   # tests that a source-distribution can be generated

# The following scriptlet checks that the resulting source distribution can be built & installed
 - export SRC_TGZ=$(cabal-1.18 info . | awk '{print $2 ".tar.gz";exit}') ;
   cd dist/;
   if [ -f "$SRC_TGZ" ]; then
      cabal-1.18 install "$SRC_TGZ";
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

### GHC HEAD Snapshots


 - Snapshots of current GHC development snapshots from the `master` branch (aka *GHC HEAD*) are uploaded at irregular intervals to the PPA
 - You can select *GHC HEAD* at your own risk by setting `GHCVER=head`
 - As GHC HEAD is experimental and likely to cause build failures, you might want to [tolerate failures](http://about.travis-ci.org/docs/user/build-configuration/#Rows-That-are-Allowed-To-Fail) by adding the following snippet to your `.travis.yml`:

    ```yaml
    matrix:
      allow_failures:
       - env: GHCVER=head
    ```

Random Remarks
--------------

 - If you want to know which core library version each GHC used (e.g. for deciding on what upper/lower bounds to declare for `build-depends`), see [GHC Boot Library Version History](http://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory)
 - Supporting GHC versions prior to 7.0.1 requires more effort:
    - GHC 7.0.1 was the first version to support `default-language: Haskell2010`
    - Declaring `cabal-version >= 1.10` makes it more difficult to compile with GHC 6.12.3's default `cabal-install`
    - `cabal-install` [falls back to top-down solver for GHC < 7](http://stackoverflow.com/questions/16021645/what-does-cabals-warning-falling-back-to-topdown-solver-for-ghc-7-mean) which may require additional tweaks to the build script to compensate for (e.g. installing `QuickCheck` via `cabal install --only-dep` is known to fail)


Real-world Examples
-------------------

 - [deepseq-generics](https://github.com/hvr/deepseq-generics) [![Build Status](https://travis-ci.org/hvr/deepseq-generics.png?branch=master)](https://travis-ci.org/hvr/deepseq-generics)
 - [filepath](https://github.com/ghc/packages-filepath)  [![Build Status](https://travis-ci.org/ghc/packages-filepath.png)](https://travis-ci.org/ghc/packages-filepath)
