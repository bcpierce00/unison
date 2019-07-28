FROM debian:stable-slim as builder

RUN set -ex; \
  apt-get -y update; \
  apt-get -y install build-essential opam; \
  eval $(opam config env)

ADD . /usr/src/unison

RUN set -ex; \
  cd /usr/src/unison; \
  make

FROM debian:stable-slim
COPY --from=builder /usr/src/unison/src/unison* /usr/local/bin/

ENTRYPOINT ["unison"]
CMD ["-doc", "about"]
