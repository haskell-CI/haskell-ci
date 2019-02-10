build:
	cabal new-build -w ghc-8.4.4 --enable-tests --constraint='Cabal^>=2.4'

ghcid :
	ghcid -c 'cabal new-repl'

install : build
	cp $$(cabal-plan list-bin make-travis-yml) $(HOME)/.local/bin/haskell-ci

test : build
	cabal new-run -w ghc-8.4.4 --enable-tests golden

accept : build
	cabal new-run -w ghc-8.4.4 --enable-tests golden -- --accept

doctest :
	doctest --fast -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XDeriveGeneric src
