FROM haskell:latest

WORKDIR /usr/src/app 

RUN stack setup --install-ghc

RUN mkdir -p build runnable

RUN stack install cryptonite

COPY src/Main.hs src/Parser.hs src/Markdown.hs src/Types.hs ./src/

COPY test/simple_features.md ./test/

COPY makefile ./

RUN make run

ENTRYPOINT ["cat", "test/output.html"]

#ENTRYPOINT [ "ls", "-la" ]
