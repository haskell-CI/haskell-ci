TAG=1

COMPILERS=ghc-8.8.1 ghc-8.6.5 ghc-8.4.4 ghc-8.2.2 ghc-8.0.2 ghc-7.10.3 ghc-7.8.4 ghc-7.6.3 ghc-7.4.2 ghc-7.0.4

IMAGES=generic-image $(COMPILERS:%=%-image)

.PHONY : all clean images push-images
.PRECIOUS : generic/Dockerfile $(COMPILERS:%=%/Dockerfile)

all : images

clean :
	rm -f $(IMAGES)

generic/Dockerfile : Dockerfile.template
	mkdir -p generic
	cat $<  | sed 's/^__.*//' > $@

%/cabal.config : cabal.config
	cp cabal.config $@

%/Dockerfile : Dockerfile.template
	mkdir -p $*
	cat $<  | sed 's/__HC__/$*/g' | sed 's/^__ //' > $@

images : $(IMAGES)

# Strips __ section
generic-image : generic/Dockerfile generic/cabal.config
	docker build -t registry.gitlab.com/haskell-ci/haskell-ci/generic generic
	docker build -t registry.gitlab.com/haskell-ci/haskell-ci/generic:1 generic
	touch $@

# Uses __ section and rewrites __HC__ to the actual compiler
%-image : %/Dockerfile %/cabal.config
	docker build -t registry.gitlab.com/haskell-ci/haskell-ci/$* $*
	docker build -t registry.gitlab.com/haskell-ci/haskell-ci/$*:$(TAG) $*
	touch $@

push-images : images
	for HC in generic $(COMPILERS); do \
		docker push registry.gitlab.com/haskell-ci/haskell-ci/$$HC; \
		docker push registry.gitlab.com/haskell-ci/haskell-ci/$$HC:$(TAG); \
	done
