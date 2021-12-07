#! /usr/bin/env sh
mkdir -p lib
scala-cli package adventofcode/ -o lib/adventofcode.jar --library -f
scala-cli setup-ide .
