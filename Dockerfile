# FROM fpco/stack-build:lts-16.31 as build

# RUN mkdir /opt/build
# COPY . /opt/build
# RUN cd /opt/build && stack install

# FROM ubuntu:20.10 as pomoshtnik
# RUN mkdir -p /opt/pomoshtnik
# WORKDIR /opt/pomoshtnik
# #RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
# COPY --from=build /root/.local/bin/pomoshtnik-exe pomoshtnik

# CMD ["/opt/pomoshtnik/pomoshtnik"]
# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker

FROM fpco/stack-build:lts-16.31 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update && apt-get download libgmp10 ca-certificates openssl
RUN mv libgmp*.deb libgmp.deb
RUN mv ca-certificates*.deb ca-certificates.deb
RUN mv openssl*.deb openssl.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-16.31 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# Running the actual application
FROM ubuntu:20.10 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY --from=dependencies /opt/build/libgmp.deb /opt/build/ca-certificates.deb /opt/build/openssl.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb
# for HTTPS
RUN dpkg -i /tmp/openssl.deb && rm /tmp/openssl.deb
RUN dpkg -i /tmp/ca-certificates.deb && rm /tmp/ca-certificates.deb

COPY --from=build /opt/build/bin .

CMD ["/opt/app/pomoshtnik-exe"]
