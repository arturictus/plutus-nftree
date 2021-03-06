FROM nixos/nix:latest

RUN echo "substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/" >> /etc/nix/nix.conf
RUN echo "trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" >> /etc/nix/nix.conf

# Fix wget cabal error: https://github.com/haskell/cabal/issues/6126r
RUN   apk update \                                                                                                                                                                                                                        
  &&   apk add ca-certificates wget \                                                                                                                                                                                                      
  &&   update-ca-certificates   # This line may not do anything

RUN apk add git


WORKDIR /opt
RUN git clone https://github.com/input-output-hk/plutus
# Just to change something so the fetch and rebase runs
ENV WEEK=06
RUN cd plutus && git fetch && git rebase
RUN cd plutus && git checkout 2d2a5da2a73a5d1789f8f789ec034ab978754f11

WORKDIR /opt/plutus

#######################
#     PLAYGROUND      #
#######################

RUN nix-shell --run "exit"
WORKDIR /opt/plutus/plutus-playground-client
# TODO this should not be done on build, create scripts to launch the processes
RUN sed -i -e 's/localhost:8080/server:8080/g' default.nix 
RUN sed -i -e 's/localhost:8080/server:8080/g' webpack.config.js
# RUN sed -i -e 's/--progress/--progress --host=0.0.0.0/g' package.json

WORKDIR /opt/plutus
RUN nix-shell --run "cd plutus-playground-client && npm install && plutus-playground-generate-purs && npm run purs:compile && npm run webpack"

EXPOSE 8080
EXPOSE 8009
# CMD nix-shell --run "cd plutus-playground-client && plutus-playground-server"
# CMD nix-shell --run "cd plutus-playground-client && npm run start"

#######################
#     CABAL BUILD     #
#######################

# COPY . /app

# RUN nix-shell --run "cd /app/code/week06 && cabal update && cabal build"

CMD ["nix-shell"]
# CMD ["bin/sh"]
