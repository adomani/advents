#! /bin/bash

currYear="$(date -d 'today - 10 month' +%Y)"
AoCyear="Advents/AoC${currYear}"

##  `getDat <d?>` takes an optional input `<d?>`.
##  If `<d?>` is provided, then it returns `<d?>`.
##  If `<d?>` is not provided, it returns one more than the number
##  in the last file named `Advents/AoC<year>/day[0]*<further_digits>.input`.
getDay () {
(
  if [ -z "${1}" ]; then
    ind="$( ls "${AoCyear}/day*.input" | tail -1 | sed 's=.*y[0]*\([0-9]*\).*=\1=' )"
    ind=$((ind+1))
  else ind="${1}"
  fi
  printf '%s' "${ind}"
)
}

##  `newday <n>` downloads the `day/n/input` from `Advent of Code`
##  and saves it to `Advents/AoC<year>/i<0n>.txt`, where `<0n>` is a 2-digit
##  representation of `<n>`, padded with zeros.
##  `newday` with no input makes a guess as to what input to download
##  choosing the smallest index larger than the largest file `Advents/AoC<year>/i<val>.txt`.
newday () {
  (
    croot;
    ind="$( getDay "${1}" )"
    if [ ! "$(git rev-parse --abbrev-ref HEAD)" == "master" ];
    then git switch master; fi
    ind0="$( printf '%02d' "${ind}" )"
    fname="${AoCyear}/day${ind0}"
    touch "${fname}.input"
    sed "s=<newDay>=${ind0}=" template.lean >> "${fname}.lean"
    brown 'Used day '; printf '%s\n' "${ind}"
#    wget "https://adventofcode.com/${currYear}/day/${ind}/input" -O "${AoCyear}/day"$( printf '%02d' "${ind}")".input"
  )
}

##  `desc_tests` prints to stdout the text that makes up the file `descriptions_with_tests.md`.
desc_tests () {
(
  croot ; cd ${AoCyear} || return 1
  descFile=.src/desc.txt
  for d in day*.lean; do
    if [ ! "${d}" == "day02.lean" ] && [ ! "${d}" == "day02_syntax.lean" ]; then
    dig=$( printf '%s' "${d}" | sed 's=day[0]*\([0-9]*\).*\.lean=\1=')
    desc="$(
      awk -v day="${dig}" 'BEGIN{ con=0 }
        !/^-- Day [0-9]*$/ && (con == day) { print $0 }
        /^-- Day [0-9]*$/ { con++ }' ../"${descFile}"
      )"
    printf '#  [Day %s](https://adventofcode.com/${currYear}/day/%s)\n\n%s\n\n' "${dig}" "${dig}" "$(
      printf '%s' "${desc}" | head -1
    )"
    awk '
      /def test/ { inside=1 }
      (inside == 1) { acc=acc "\n" $0 }
      /[^"]*"$/ { inside=0 }
      END{ print acc }' "${d}"
    printf -- '\n%s\n\n[%s](%s)\n\n---\n\n' "$(
      printf "${desc}\n" | tail -n+2
    )" "Solution in Lean" "${AoCyear}/${d/_traditional/}"
  fi
  done | sed '
      s=def test\([0-9]*\)[^"]*["]*=\n####  Test \1\n\n<pre>\n=g
      s="=\n</pre>=
    ' |
    sed -z '
      s=\n\n[\n]*=\n\n=g
      s=\n[\n]*</pre>=\n</pre>=g
      s=  *\n=\n=g
      s=\n[\n]*$=\n=
    '
)
}

##  `desc` prints to stdout the text that makes up the file `descriptions.md`.
desc () {
(
  croot
  awk -v fil='descriptions_with_tests.md' 'BEGIN {
      con=1
      acc=""
      print "|Day|Description|\n|:-:|-|"
      link=sprintf("(%s#day-", fil)
    }
    2 <= NR && /^-- Day [0-9]*$/ {
      printf("|[%s]%s%s)|%s|\n", con, link, con, acc)
      con++
      acc=""
    }
    !/^-- Day [0-9]*$/ && (acc == "") { acc=$0 }
    END { printf("|[%s]%s%s)|%s|\n", con, link, con, acc) }' .src/desc.txt |
      column -s'|' -o'|' -t | sed 's=|= | =g; s=^ ==; s= $=='
)
}

##  Creates the files `descriptions_with_tests.md` and `descriptions.md`,
##  overwriting them if they already exist.
aoc () {
(
  croot
  desc_tests > descriptions_with_tests.md
  desc > descriptions.md
)
}

##  An auxilliary function for processing all the `dayXX.lean` files
##  with an option of timing the individual runs.
leanWith () {
(
  croot || exit 1
  for fil in "${AoCyear}"/day??.lean; do
    nm="$(printf ' %s' "${fil}" | sed 's=.*\(day[0-9]*\).*=\1=' )"
    brown 'Process '; lcyan "${nm}"$'\n'
    if [ -z "${1}" ]
      then lake env lean "${fil}"
      else time lake env lean "${fil}"
    fi
  done
)
}

##  Processes all the `dayXX.lean` files.
leanall () { leanWith; }

##  Processes all the `dayXX.lean` files, returning timing information for each.
leantime () { leanWith 'time'; }
