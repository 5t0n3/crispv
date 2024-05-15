FROM alpine:3.18 AS builder

# install ghcup (also internally runs cabal update it seems)
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_MINIMAL=1

RUN apk add --no-cache binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/root/.ghcup/bin:$PATH"

# cabal-install 3.12.0.0-prerelease fixes weirdness with cabal configure + install :P
# (3.11 in filename is because it's a prerelease)
RUN ghcup --no-cache install ghc --set latest
RUN ghcup --no-cache install cabal -u "https://gitlab.haskell.org/haskell/cabal/-/jobs/1848323/artifacts/raw/out/cabal-install-3.11.0.0-x86_64-linux-alpine3_18.tar.xz" 3.12.0.0-prerelease

# extra system dependencies
RUN apk add --no-cache zlib-dev zlib-static libc++-static gmp-dev

# update cabal package index
RUN cabal update

# dependency caching yay
WORKDIR /opt/crispv
COPY crispv.cabal .
RUN cabal configure --enable-executable-static --enable-executable-stripping
RUN cabal build --only-dependencies -j4

# actually build crispv
COPY . .
RUN cabal install

# actual image with just compiled server binary
FROM scratch

# copy over server binary from build container
WORKDIR /srv/crispv
COPY --from=builder /root/.local/bin/crispv crispv

# and run it on container start :)
CMD ["/srv/crispv/crispv"]

# the server listens on port 8000
EXPOSE 8000
