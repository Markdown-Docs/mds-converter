FROM haskell:latest

WORKDIR /usr/app 

RUN stack setup --install-ghc

RUN mkdir -p build runnable

RUN stack install cryptonite

COPY src/Main.hs src/Parser.hs src/Markdown.hs src/Types.hs ./src/

COPY test/simple_features.md ./test/

COPY makefile ./

RUN make build

EXPOSE 3000

ENTRYPOINT ["make"]