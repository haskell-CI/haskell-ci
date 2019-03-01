# Build docker images

Current registry is: https://gitlab.com/haskell-ci/haskell-ci/container_registry

We don't build images for each compiler, only for latest in series (for now)

All images are built from the same template, `Dockerfile.template`
We install few C `-dev` dependencies. Make a PR if you need some more.

There's also a `generic` image without `ghc` and `cabal`, which is used
in cases where there are no image for that ghc&cabal pair.

- `make all` will build images
- `make push-images` will also push them to the registry
- `make ghc-8.4.4-image` or `make generic-image` to build single image.
   This is useful when making changes (and checking the size of resulting images)

## Misc

Docker cleanup:

```
docker system prune --volumes
```
