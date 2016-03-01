#!/usr/bin/env bash
#

set -ue
sh -x .travis-ocaml.sh 2>travis.log
. <( opam config env )

set -x
make
make test
