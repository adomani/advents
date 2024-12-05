#!/bin/bash

year=2021

awk -v year="${year}" 'BEGIN{should_print=0}
  /^namespace Day[0-9]*$/ {
    day=$0
    gsub(/namespace Day[0]*/, "", day)
    aoc_ref=sprintf("#  [Day %s](https://adventofcode.com/%s/day/%s)", day, year, day)
  }

  /^-\/$/ {should_print=0}
  (should_print == 2) {module_docs=module_docs "\n" $0}
  /^\/-!/ {should_print++}
  (($0 ~ "Day "day) && (should_print == 1)) {should_print++}

  /def test/ { inside=1 }
  (inside == 1) { acc=acc "\n" $0 }
  /[^"]*"$/ { inside=0 }
  END{
    printf aoc_ref
    printf module_docs
    printf acc }

' ~/advents/Advents/AoC2024/day05.lean
