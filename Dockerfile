FROM markhibberd/boris-build:latest as builder

ENV LANG C.UTF-8

COPY . /build/

WORKDIR /build

RUN cabal update

RUN ./mafia build

FROM debian:stretch

ENV LANG C.UTF-8

RUN apt-get update && \
  apt-get install -y \
    libgmp-dev \
    libz-dev

COPY --from=builder /build/dist/build/quote-bot/quote-bot /usr/bin/quote-bot
