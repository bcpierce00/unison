#!/usr/bin/env bash
#

set -ue
sh -x .travis-ocaml.sh &>travis.log
. <( opam config env )

set -x
make
make test
