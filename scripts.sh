#! /bin/bash

newday () {
  (
    croot;
    git switch master
    wget "https://adventofcode.com/2023/day/${1}/input" -O "Advents/i"$( printf '%02d' "${1}")".txt"
  )
}
