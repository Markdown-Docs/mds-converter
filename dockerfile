FROM haskell:latest

WORKDIR /usr/src/app 

RUN stack setup --install-ghc

RUN mkdir -p build runnable

COPY src/Main.hs .

RUN stack ghc -- Main.hs -hidir build -odir build -o runnable/hello

CMD ["./runnable/hello"]
