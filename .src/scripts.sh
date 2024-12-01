#! /bin/bash

currYear="$(date -d 'today - 10 month' +%Y)"
rootDir='Advents/AoC'

##  `getDat <d?> <year?>` takes an optional input `<d?>` and `<year?>`.
##  If `<d?>` is provided, then it returns `<d?>`.
##  If `<d?>` is not provided, it returns one more than the number
##  in the last file named `Advents/AoC<year>/day[0]*<further_digits>.input`.
getDay () {
(
  if [ -z "${1}" ]; then
    yr="${2:-$currYear}"
    AoCyear="${rootDir}${yr}"
    ind="$( ls "${AoCyear}"/day*.input | tail -1 | sed 's=.*y[0]*\([0-9]*\).*=\1=' )"
    ind=$((ind+1))
  else ind="${1}"
  fi
  printf '%s' "${ind}"
)
}

##  `newday <n> <year>` creates `Advents/AoC<year>/day<0n>.{lean, input}`, where `<0n>` is a 2-digit
##  representation of `<n>`, padded with zeros.
##  `newday` with no input makes a guess as to what the day and year are,
##  choosing the smallest index larger than the largest file `Advents/AoC<year>/day<val>.input`.
newday () {
  (
    yr="${2:-$currYear}"
    AoCyear="${rootDir}${yr}"
    croot;
    ind="$( getDay "${1}" "${yr}")"
    if [ ! "$(git rev-parse --abbrev-ref HEAD)" == "master" ];
    then git switch master; fi
    ind0="$( printf '%02d' "${ind}" )"
    fname="${AoCyear}/day${ind0}"
    touch "${fname}.input"
    sed "s=_newDay_=${ind0}=; s=YYYY=${yr}=g" .src/template.lean >> "${fname}.lean"
    printf 'import %s\n' "${fname//\//.}" >> Advents.lean
    brown 'Used day '; printf '%s\n' "${ind}"
#    wget "https://adventofcode.com/${yr}/day/${ind}/input" -O "${AoCyear}/day"$( printf '%02d' "${ind}")".input"
  )
}

##  `desc_tests` prints to stdout the text that makes up the file `descriptions_with_tests.md`.
desc_tests () {
(
  yr="${1:-$currYear}"
  AoCyear="${rootDir}${yr}"
  croot
  baseDir="$(pwd)"
  cd ${AoCyear} || return 1
  descFile=.src/"${yr}"_desc.txt
  for d in day*.lean; do
    if [ ! "${d}" == "day02.lean" ] && [ ! "${d}" == "day02_syntax.lean" ]; then
    dig=$( printf '%s' "${d}" | sed 's=day[0]*\([0-9]*\).*\.lean=\1=')
    desc="$(
      awk -v day="${dig}" 'BEGIN{ con=0 }
        !/^-- Day [0-9]*$/ && (con == day) { print $0 }
        /^-- Day [0-9]*$/ { con++ }' "${baseDir}/${descFile}"
      )"
    printf '#  [Day %s](https://adventofcode.com/%s/day/%s)\n\n%s\n\n' "${dig}" "${yr}" "${dig}" "$(
      printf '%s' "${desc}" | head -1
    )"
    awk '
      /def test/ { inside=1 }
      (inside == 1) { acc=acc "\n" $0 }
      /[^"]*"$/ { inside=0 }
      END{ print acc }' "${d}"
    printf -- '\n%s\n\n[%s](%s)\n\n---\n\n' "$(
      printf "${desc}\n" | tail -n+2
    )" "Solution in Lean" "${d/_traditional/}"
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
##  Passing an optional input makes that input the prefx of the file.  This is typically
##  a year, such as 2023.  The default value is the "current" year, which is the year that
##  it was 10 months ago.
desc () {
(
  yr="${1:-$currYear}"
  AoCyear="${rootDir}${yr}"
  croot
  awk -v fil="${yr}_descriptions_with_tests.md" 'BEGIN {
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
    END { printf("|[%s]%s%s)|%s|\n", con, link, con, acc) }' .src/"${yr}"_desc.txt |
      column -s'|' -o'|' -t | sed 's=|= | =g; s=^ ==; s= $=='
)
}

##  Creates the files `descriptions_with_tests.md` and `descriptions.md`,
##  overwriting them if they already exist.
aoc () {
(
  yr="${1:-$currYear}"
  AoCyear="${rootDir}${yr}"
  croot
  desc_tests "${yr}" > "${AoCyear}"/${yr}_descriptions_with_tests.md
  desc "${yr}" > "${AoCyear}"/${yr}_descriptions.md
)
}

##  An auxilliary function for processing all the `dayXX.lean` files
##  with an option of timing the individual runs.
leanWith () {
(
  yr="${2:-$currYear}"
  AoCyear="${rootDir}${yr}"
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

##  `leantime` processes all the `dayXX.lean` files of the current year,
##  returning timing information for each.
##  `leantime YYYY` processes all the `dayXX.lean` files of the given year.
leantime () { leanWith 'time' "${1}"; }
