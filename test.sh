#! /usr/bin/env sh
scala-cli test "year$1" -- -o "adventofcode.Year$1Suite.day $2 *"