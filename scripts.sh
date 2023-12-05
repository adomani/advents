#! /bin/bash

##  `newday <n>` downloads the `day/n/input` from `Advent of Code`
##  and saves it to `Advents/i<0n>.txt`, where `<0n>` is a 2-digit
##  representation of `<n>`, padded with zeros.
##  `newday` with no input makes a guess as to what input to download
##  choosing the smallest index larger than the largest file `Advents/i<val>.txt`.
newday () {
  (
    if [ -z "${1}" ]; then
      ind="$( ls Advents/day*.input | tail -1 | sed 's=.*y[0]*\([0-9]*\).*=\1=' )"
      ind=$((ind+1))
    else ind="${1}"
    fi
    croot;
    git switch master
    wget "https://adventofcode.com/2023/day/${ind}/input" -O "Advents/day"$( printf '%02d' "${ind}")".input"
  )
}
