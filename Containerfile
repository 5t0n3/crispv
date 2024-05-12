FROM haskell:slim

RUN cabal update

WORKDIR /opt/crispv

COPY crispv.cabal .

RUN cabal build --only-dependencies -j4

COPY . .
RUN cabal install

CMD ["crispv"]
