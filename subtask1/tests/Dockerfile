# tests/Dockerfile
FROM haskell:9.2.5

WORKDIR /app
COPY . /app

RUN cabal update && cabal install --only-dependencies && cabal build

CMD ["cabal", "run", "editor", "solution/data/sample.hs"]
