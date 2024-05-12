FROM haskell:slim

RUN cabal update

# dependency caching yay
WORKDIR /opt/crispv
COPY crispv.cabal .
RUN cabal build --only-dependencies -j4

# actually build crispv
COPY . .
RUN cabal install

# and run it :)
CMD ["crispv"]
