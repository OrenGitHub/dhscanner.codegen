FROM haskell:9.8.1
RUN cabal update
RUN cabal install cabal-install
RUN apt-get update
RUN apt-get install tree -y
RUN apt-get install vim -y
RUN echo "set number" > ~/.vimrc
RUN echo "set incsearch" >> ~/.vimrc
RUN echo "syntax on" >> ~/.vimrc
WORKDIR /codegen
COPY . .