FROM haskell:9.4

WORKDIR /workspace
COPY . .

RUN ghc main.hs Lib.hs

CMD ["./main"]
