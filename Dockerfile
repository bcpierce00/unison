FROM debian:stable-slim as builder

ENV VERSION 2.51.2

RUN set -ex; \
  apt-get -y update; \
  apt-get -y install curl build-essential opam; \
  \
  mkdir /usr/src/unison && cd /usr/src/unison; \
  curl -Ls https://github.com/bcpierce00/unison/archive/v${VERSION}.tar.gz |tar xz --strip 1; \
  \
  make

FROM debian:stable-slim
COPY --from=builder /usr/src/unison/src/unison* /usr/local/bin/

ENTRYPOINT ["unison"]
CMD ["-doc", "about"]
