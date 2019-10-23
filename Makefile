HC ?= ghc-8.6.5

build :
	cabal v2-build -w $(HC)

self-test :
	cabal v2-run -w $(HC) haskell-ci -- --config=cabal.haskell-ci haskell-ci.cabal

ghcid :
	ghcid -c 'cabal v2-repl -w $(HC)'

install:
	cabal v2-install -w $(HC) haskell-ci:exe:haskell-ci --overwrite-policy=always

install-dev : build
	cp $$(cabal-plan list-bin haskell-ci) $(HOME)/.cabal/bin/haskell-ci

test : build
	cabal v2-run -w $(HC) golden

accept : build
	cabal v2-run -w $(HC) golden -- --accept

doctest :
	doctest --fast -XBangPatterns -XScopedTypeVariables -XDerivingStrategies -XGeneralizedNewtypeDeriving -XDeriveAnyClass -XNoImplicitPrelude -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XDeriveGeneric src
