FROM haskell:8.10

WORKDIR /opt/app

RUN cabal update

COPY ./aoc.cabal /opt/app/aoc.cabal

RUN cabal build --only-dependencies -j4

COPY . /opt/app
RUN cabal install

CMD ["main"]
