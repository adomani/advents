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

desc () {
(
  croot ; cd Advents || return 1
  descFile=descriptions.md
  for d in day*.lean; do
    ##brown "${d}"$'\n'
    if [ ! "${d}" == "day02.lean" ] && [ ! "${d}" == "day02_syntax.lean" ]; then
    dig=$( printf '%s' "${d}" | sed 's=day[0]*\([0-9]*\).*\.lean=\1=')
    desc="$( sed -n "s=| *${dig} *|\([^|]*\).*=\1=p" ../"${descFile}" )"
    printf '#  Day %s\n\n%s\n\n' "${dig}" "${desc}"
    awk '
      /def test/ { inside=1 }
      (inside == 1) { acc=acc "\n" $0 }
      /[^"]*"$/ {inside=0}
      END{ print acc }' "${d}"
    printf -- '\n---\n\n'
  fi
  done | sed '
      s=def test\([12]*\)[^"]*["]*=<pre>\nTest \1\n\n=g
      s="=\n</pre>=
    ' |
    sed -z '
      s=\n\n[\n]*=\n\n=g
      s=\n[\n]*</pre>=\n</pre>=g
    ' > ../descriptions_with_tests.md
)
}
