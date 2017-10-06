FROM ubuntu:14.04

RUN apt-get -y update \
  && apt-get install -y \
  build-essential \
  software-properties-common \
  && add-apt-repository --yes ppa:avsm/ppa \
  && apt-get update -qq \
  && apt-get install -y opam build-essential \
  && eval $(opam config env)

ADD . /tmp/unison

RUN cd /tmp/unison \
  && make \
  && cp src/unison /usr/local/bin \
  && cp src/unison-fsmonitor /usr/local/bin \
  && rm -rf /tmp/unison

ENTRYPOINT ["unison"]
