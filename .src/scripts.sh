#! /bin/bash

getDay () {
(
  if [ -z "${1}" ]; then
    ind="$( ls Advents/day*.input | tail -1 | sed 's=.*y[0]*\([0-9]*\).*=\1=' )"
    ind=$((ind+1))
  else ind="${1}"
  fi
  printf '%s' "${ind}"
)
}

##  `newday <n>` downloads the `day/n/input` from `Advent of Code`
##  and saves it to `Advents/i<0n>.txt`, where `<0n>` is a 2-digit
##  representation of `<n>`, padded with zeros.
##  `newday` with no input makes a guess as to what input to download
##  choosing the smallest index larger than the largest file `Advents/i<val>.txt`.
newday () {
  (
    croot;
    ind="$( getDay "${1}" )"
    git switch master
    ind0="$( printf '%02d' "${ind}" )"
    fname="Advents/day${ind0}"
    touch "${fname}.input"
    sed "s=<newDay>=${ind0}=" template.lean >> "${fname}.lean"
#    wget "https://adventofcode.com/2023/day/${ind}/input" -O "Advents/day"$( printf '%02d' "${ind}")".input"
  )
}

desc_tests () {
(
  croot ; cd Advents || return 1
  descFile=.src/desc.txt
  for d in day*.lean; do
    ##brown "${d}"$'\n'
    if [ ! "${d}" == "day02.lean" ] && [ ! "${d}" == "day02_syntax.lean" ]; then
    dig=$( printf '%s' "${d}" | sed 's=day[0]*\([0-9]*\).*\.lean=\1=')
    desc="$(
      awk -v day="${dig}" 'BEGIN{ con=1 }
        !/^--$/ && (con == day) { print $0 }
        /^--$/ { con++ }' ../"${descFile}"
      )"
    printf '#  Day %s\n\n%s\n\n' "${dig}" "${desc}"
    awk '
      /def test/ { inside=1 }
      (inside == 1) { acc=acc "\n" $0 }
      /[^"]*"$/ { inside=0 }
      END{ print acc }' "${d}"
    printf -- '\n---\n\n'
  fi
  done | sed '
      s=def test\([12]*\)[^"]*["]*=\n####  Test \1\n\n<pre>\n=g
      s="=\n</pre>=
    ' |
    sed -z '
      s=\n\n[\n]*=\n\n=g
      s=\n[\n]*</pre>=\n</pre>=g
      s=  *\n=\n=g
    '
)
}

desc () {
(
  croot
  awk -v fil='descriptions_with_tests.md' 'BEGIN {
      con=1
      acc=""
      print "|Day|Description|\n|:-:|-|"
      link=sprintf("(%s#day-", fil)
    }
    /^--$/ {
      printf("|[%s]%s%s)|%s|\n", con, link, con, acc)
      con++
      acc=""
    }
    !/^--$/ { acc=$0 }' .src/desc.txt
)
}

aoc () {
(
  croot
  desc_tests > descriptions_with_tests.md
  desc > descriptions.md
)
}

leanall () {
(
  croot || exit 1
  for fil in Advents/day??.lean; do
    nm="$(printf ' %s' "${fil}" | sed 's=.*\(day[0-9]*\).*=\1=' )"
    brown 'Process '; lcyan "${nm}"$'\n'
    lake env lean "${fil}"
  done
)
}
