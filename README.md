haskell-ci - CI generator for multiple [GHC](http://haskell.org/ghc) versions
=============================================================================

At the moment `haskell-ci` support GitHub Actions workflow generation.
There is also legacy Travis-CI configuration generator, which is unmaintained.

`haskell-ci` relies on [`hvr-ppa`](https://launchpad.net/~hvr/+archive/ubuntu/ghc)
or [`ghcup`](https://www.haskell.org/ghcup/) to install GHC
and `cabal-install`.

### Quick-start instructions

* Step 1: Clone and install this project in/from any directory

    ```bash
    $ git clone https://github.com/haskell-CI/haskell-ci.git
    $ cd haskell-ci
    $ cabal install haskell-ci:exe:haskell-ci
    ```

  or

    ```bash
    cabal install haskell-ci
    ```

* Step 2: Change directories to your project:

    ```bash
    $ cd path/to/your-project
    ```

* Step 3: Edit your project's `*.cabal` file to add a `tested-with` line, such as this one:

    ```bash
    $ cat your-project.cabal
    ...
    tested-with: GHC ==9.6.3 || ==9.4.8 || ==9.2.8
    ...
    ```
    
    Add as many or as few GHC versions to test as you want.

* Step 4: Generate a workflow file for your project:

    ```bash
    $ # You run the following command from your project's directory, even
    $ # though it references the script from the `haskell-ci` project
    $ haskell-ci github your-project.cabal
    ```

    Note: If you have multiple local Cabal projects that you wish to build together
    using a `cabal.project` file, pass that file to haskell-ci instead:
    ```bash
    $ haskell-ci github cabal.project
    ```

    The `haskell-ci` tool looks at the `Tested-With` line in your
    `*.cabal` files and generates a configuration that tests each compiler
    version you listed in parallel.

* Step 5: Create a pull request with your new CI configuration.

    ... or push directly to your main branch if you feel lucky.
    
    Sometimes you may need to regenerate CI script, for example, when
    adding new compiler version to `tested-with`.
    You may simply run `haskell-ci regenerate`.

Real-world Examples
-------------------

 - [aeson](https://github.com/haskell/aeson)
 - [lens](https://github.com/ekmett/lens)
 - [unordered-containers](https://github.com/haskell-unordered-containers/unordered-containers)
