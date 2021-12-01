FROM haskell:8.10

WORKDIR /opt/app

RUN cabal update

COPY ./day01.cabal /opt/app/day01.cabal

RUN cabal build --only-dependencies -j4

COPY . /opt/app
RUN cabal install

CMD ["main"]
