build :
	cabal new-build -w ghc-8.4.4 --enable-tests

self-test :
	cabal new-run -w ghc-8.4.4 haskell-ci -- --config=cabal.haskell-ci haskell-ci.cabal

ghcid :
	ghcid -c 'cabal new-repl'

install:
	cabal new-install haskell-ci:exe:haskell-ci

install-dev : build
	cp $$(cabal-plan list-bin haskell-ci) $(HOME)/.local/bin/haskell-ci

test : build
	cabal new-run -w ghc-8.4.4 --enable-tests golden

accept : build
	cabal new-run -w ghc-8.4.4 --enable-tests golden -- --accept

doctest :
	doctest --fast -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XDeriveGeneric src
