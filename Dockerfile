FROM debian:stable-slim as builder

RUN set -ex; \
  apt-get -y update; \
  apt-get -y install curl build-essential opam

COPY . /usr/src/unison

RUN set -ex; \
  cd /usr/src/unison; \
  make

FROM debian:stable-slim
COPY --from=builder /usr/src/unison/src/unison* /usr/local/bin/

ENV HOME /home/user
RUN useradd --create-home --home-dir $HOME user \
	&& mkdir $HOME/documents && chown -R user:user $HOME

WORKDIR $HOME
USER user

ENTRYPOINT ["unison"]
CMD ["-doc", "about"]
