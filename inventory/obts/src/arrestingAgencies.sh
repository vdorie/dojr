#!/bin/bash

## used to extract arresting agencies from the OBTS manual
## uses the utility 'pdftotext'

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 path/to/manual.pdf" >&2
  exit 1
fi
if ! [ -e "$1" ]; then
  echo "$1 not found" >&2
  exit 1
fi
if ! [ -f "$1" ]; then
  echo "$1 not a file" >&2
  exit 1
fi

pdftotext -layout -f 13 -l 50 "$1" agencies.txt
grep -e '^[[:alnum:]]\+' agencies.txt > agencies2.txt

## OS X's grep reaallly really sucks
grep -E -e '^[^A][^R][^B]' agencies2.txt > agencies.txt
rm agencies.txt

## this line splits when there are two or more spaces and replaces with a comma
perl -pe 's/^(\S+)\s+(\S+(?:\s\S+)*)\s{2,}(\S+(?:\s\S+)*)$/\1,\2,\3/' agencies.txt > arresting_agencies.csv

## add a header
printf '0a\ncode,agency,type\n.\nw\n' | ed arresting_agencies.csv 
rm agencies.txt

echo "output created as arresting_agencies.csv"

exit 0
