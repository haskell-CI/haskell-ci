FROM ubuntu:16.04 AS matrix-worker-base
RUN apt-get update && apt-get install -y gnupg
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > /etc/apt/sources.list.d/ghc.list && \
    echo 'deb http://ppa.launchpad.net/hvr/matrix.hackage/ubuntu xenial main' > /etc/apt/sources.list.d/hackageci.list
RUN apt-key adv --keyserver keyserver.ubuntu.com  --recv-keys FF3AEACEF6F88286
RUN echo 1 && apt-get update && apt-get dist-upgrade -y && apt-get autoremove -y --purge && apt-get clean
# locale stuff
RUN apt-get -y install locales && sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen && apt-get clean
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
# toolchain & FFI libraries [A]
RUN apt-get install -y m4 build-essential && apt-get clean
RUN apt-get install -y \
        freeglut3-dev \
        libadns1-dev \
        libasound2-dev \
        libbibutils-dev \
        libblas-dev \
        libbz2-dev \
        libcairo2-dev \
        libcrypto++-dev \
        libdevil-dev \
        libedit-dev \
        libfftw3-dev \
        libfreenect-dev \
        libfreenect-dev \
        libftgl-dev \
        libftgl-dev \
        libfuse-dev \
        libfuse-dev \
        libg15-dev \
        libgd2-xpm-dev \
        libgeoip-dev \
        libgirepository1.0-dev \
        libglew-dev \
        libglib2.0-dev \
        libglpk-dev \
        libglu1-mesa-dev \
        libgmp-dev \
        libgpgme11-dev \
        libgsl-dev \
        libgsl0-dev \
        libgtk-3-dev \
        libgtk2.0-dev \
        libhidapi-dev \
        libhidapi-dev \
        libicu-dev \
        libjack-dev \
        libjudy-dev \
        libkrb5-dev \
        liblapack-dev \
        libldap2-dev \
        libleveldb-dev \
        libleveldb-dev \
        liblmdb-dev \
        liblz4-dev \
        liblzma-dev \
        libmagic-dev \
        libmarkdown2-dev \
        libmp3lame-dev \
        libmpfr-dev \
        libmpg123-dev \
        libmysqlclient-dev \
        libncurses-dev \
        libncursesw5-dev \
        libnetcdf-dev \
        libnfc-dev \
        libnotify-dev \
        libopenal-dev \
        libopenal-dev \
        libopencv-dev \
        libpango1.0-dev \
        libpcap-dev \
        libpcre2-dev \
        libpcre3-dev \
        libpq-dev \
        libqrencode-dev \
        librdkafka-dev \
        libre2-dev \
        libsctp-dev \
        libsdl-gfx1.2-dev \
        libsdl-ttf2.0-dev \
        libsdl1.2-dev \
        libsdl2-dev \
        libsdl2-ttf-dev \
        libsecp256k1-dev \
        libsndfile1-dev \
        libsnmp-dev \
        libsnmp-dev \
        libsodium-dev \
        libsqlite3-dev \
        libssh-dev \
        libssl-dev \
        libsystemd-dev \
        libtag-extras-dev \
        libtagc0-dev \
        libtwolame-dev \
        libudev-dev \
        libvte-2.91-dev \
        libwxgtk-media3.0-dev \
        libwxgtk-webview3.0-dev \
        libxml2-dev \
        libxslt1-dev \
        libxss-dev \
        libyaml-dev \
        libz3-dev \
        libzfslinux-dev \
        libzip-dev \
        libzmq-dev \
        libzookeeper-mt-dev \
        lzma-dev \
        mpi-default-dev \
        nettle-dev \
        ruby-dev \
        unixodbc-dev \
        uuid-dev \
        zlib1g-dev \
        libgit2-dev \
        libalut-dev \
    && apt-get autoremove -y --purge && apt-get clean

# recheck for updated dists
#RUN echo 1 && apt-get update && apt-get dist-upgrade -y

# GHC toolchains
RUN apt-get install -y \
        ghc-7.0.4  \
        ghc-7.4.2  \
        ghc-7.6.3  \
        ghc-7.8.4  \
        ghc-7.10.3 \
        ghc-8.0.2  \
        ghc-8.2.2  \
        ghc-8.4.4  \
        ghc-8.6.3  \
    && apt-get clean

