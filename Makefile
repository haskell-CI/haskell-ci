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
	(cd cabal-install-parsers && cabal v2-run -w $(HC) cabal-parsers-golden -- --accept)

doctest :
	doctest --fast -XBangPatterns -XScopedTypeVariables -XDerivingStrategies -XGeneralizedNewtypeDeriving -XDeriveAnyClass -XNoImplicitPrelude -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XDeriveGeneric src

regenerate :
	cabal v2-run -w ghc-8.6.5 -- haskell-ci regenerate
