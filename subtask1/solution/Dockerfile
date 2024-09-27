# tests/Dockerfile
FROM haskell:8.8.4

WORKDIR /app
COPY . /app

RUN cabal update && cabal install --only-dependencies && cabal build

CMD ["cabal", "run", "editor", "data/sample.hs"]
