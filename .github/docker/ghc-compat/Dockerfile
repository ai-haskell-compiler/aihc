# syntax=docker/dockerfile:1.7

# GHC compatibility toolchain.
# One image holds one GHC version, Cabal, and Hackage libraries.
# The image does not compile aihc-resolve or aihc-tc.

ARG DEBIAN_IMAGE=debian:bookworm-slim@sha256:88200866dfff7ea7f5cbcb6ec7c8a701889efe6fe859fe64d6990e4b07ea4171

FROM ${DEBIAN_IMAGE} AS toolchain

ARG GHC_VERSION
ARG CABAL_VERSION=3.16.0.0
ARG GHCUP_VERSION=0.2.6.2

RUN test -n "$GHC_VERSION"

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    GHCUP_INSTALL_BASE_PREFIX=/root \
    PATH="/opt/ghc/bin:/opt/cabal-exe:${PATH}"

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      curl \
      git \
      libffi-dev \
      libgmp-dev \
      libncurses-dev \
      libnuma-dev \
      pkg-config \
      xz-utils \
      zlib1g-dev \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL "https://downloads.haskell.org/~ghcup/${GHCUP_VERSION}/x86_64-linux-ghcup-${GHCUP_VERSION}" \
      -o /usr/bin/ghcup \
 && chmod +x /usr/bin/ghcup \
 && ghcup install ghc "$GHC_VERSION" --isolate /opt/ghc \
 && ghcup install cabal "$CABAL_VERSION" --isolate /opt/cabal-exe \
 && if [ ! -x /opt/cabal-exe/cabal ]; then \
      cabal_bin="$(find /opt/cabal-exe -type f -name 'cabal*' | head -n 1)"; \
      test -n "$cabal_bin"; \
      ln -s "$cabal_bin" /opt/cabal-exe/cabal; \
    fi \
 && ghc --numeric-version \
 && cabal --numeric-version \
 && rm -rf \
      /root/.ghcup/cache \
      /root/.ghcup/tmp \
      /root/.ghcup/logs \
      /opt/ghc/share/doc \
      /opt/ghc/share/man \
 && find /opt/ghc -type f \( \
      -name '*_p.a' \
      -o -name '*.p_hi' \
      -o -name '*.p_dyn_hi' \
      -o -name '*_debug.a' \
      -o -name '*_l.a' \
    \) -delete \
 && rm -f \
      /opt/ghc/bin/haddock \
      /opt/ghc/bin/haddock-* \
      /opt/ghc/bin/hp2ps \
      /opt/ghc/bin/hpc \
      /opt/ghc/bin/hpc-*

FROM toolchain AS deps

ENV CABAL_DIR=/opt/cabal \
    DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH="/opt/ghc/bin:/opt/cabal-exe:${PATH}"

WORKDIR /src

COPY cabal.project.ghc-compat ./
COPY components/aihc-resolve/aihc-resolve.cabal components/aihc-resolve/LICENSE components/aihc-resolve/
COPY components/aihc-tc/aihc-tc.cabal components/aihc-tc/LICENSE components/aihc-tc/

RUN mkdir -p components/aihc-resolve/src components/aihc-tc/src \
 && cabal update \
 && cabal build all \
      --project-file=cabal.project.ghc-compat \
      --builddir=/tmp/dist-newstyle \
      --only-dependencies \
      --disable-tests \
      --disable-benchmarks \
      --disable-optimization \
      --disable-documentation \
      --disable-library-profiling \
      --enable-library-stripping \
      --ghc-options=-j \
 && rm -rf \
      /tmp/dist-newstyle \
      /opt/cabal/logs \
      /opt/cabal/store/ghc-*/incoming \
 && find /opt/cabal/store -type d -name doc -print0 | xargs -0 -r rm -rf

FROM ${DEBIAN_IMAGE}

ENV CABAL_DIR=/opt/cabal \
    DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH="/opt/ghc/bin:/opt/cabal-exe:${PATH}"

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      ca-certificates \
      gcc \
      g++ \
      git \
      libc6-dev \
      libffi-dev \
      libgmp-dev \
      libncurses-dev \
      libnuma1 \
      zlib1g-dev \
 && rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man

COPY --from=deps /opt/ghc /opt/ghc
COPY --from=deps /opt/cabal-exe /opt/cabal-exe
COPY --from=deps /opt/cabal /opt/cabal
