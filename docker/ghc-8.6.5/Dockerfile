FROM ubuntu:xenial
MAINTAINER Oleg Grenrus <oleg.grenrus@iki.fi>

# UTF FTW
ENV LANG C.UTF-8

# hvr-ppa and some core packages
RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends install \
    automake \
    build-essential \
    ca-certificates \
    git \
    python-software-properties \
    pkg-config \
    software-properties-common \
    sudo \
  && apt-add-repository -y "ppa:hvr/ghc" \
  && apt-get autoremove -y --purge && apt-get clean && rm -rf /var/lib/apt/lists/*

# Dependencies
RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends install \
    libbz2-dev \
    libfftw3-dev \
    libgmp-dev \
    liblapack-dev \
    libncurses-dev \
    libncursesw5-dev \
    libpq-dev \
    libsqlite3-dev \
    libssl-dev \
    lzma-dev \
    zlib1g-dev \
  && apt-get autoremove -y --purge && apt-get clean && rm -rf /var/lib/apt/lists/*

# User
RUN adduser haskellci --gecos "Haskell CI builds" --disabled-password
RUN echo "haskellci ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER haskellci
WORKDIR /home/haskellci
ENV PATH /home/haskellci/.cabal/bin:$PATH

# Cmd
CMD ["/bin/bash"]

# We install cabal-install always, also for generic image
RUN sudo apt-get -yq update && sudo apt-get -yq --no-install-suggests --no-install-recommends install \
    cabal-install-3.0 \
  && sudo apt-get autoremove -y --purge && sudo apt-get clean && sudo rm -rf /var/lib/apt/lists/* && rm -rf /home/haskellci/.cabal/packages

# Cabal config
RUN mkdir -p /home/haskellci/.cabal
ADD cabal.config /home/haskellci/.cabal/config

# TODO: when cabal-install-3.0 is out with `copy`; install cabal-plan :)

# We also perform initial v2-update
# 01-index.tar.gz is large, but rather it's downloaded during the image download
RUN /opt/ghc/bin/cabal v2-update \
  && mv /home/haskellci/.cabal/packages/hackage.haskell.org/01-index.tar.gz /home/haskellci/01-index.tar.gz \
  && rm -rf /home/haskellci/.cabal \
  && mkdir -p /home/haskellci/.cabal/packages/hackage.haskell.org \
  && mv /home/haskellci/01-index.tar.gz /home/haskellci/.cabal/packages/hackage.haskell.org/01-index.tar.gz

# Image specific GHC GHC
RUN sudo apt-get -yq update && sudo apt-get -yq --no-install-suggests --no-install-recommends install \
    ghc-8.6.5 \
  && sudo apt-get autoremove -y --purge && sudo apt-get clean && sudo rm -rf /var/lib/apt/lists/*
