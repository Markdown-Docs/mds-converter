FROM haskell:latest

WORKDIR /usr/src/app 

RUN stack setup --install-ghc

RUN mkdir -p build runnable

COPY src/Main.hs src/Parser.hs src/Markdown.hs src/Types.hs ./

RUN stack ghc -- Main.hs Parser.hs Markdown.hs Types.hs -O3 -hidir build -odir build -o runnable/parser

ENTRYPOINT ["./runnable/parser"]
