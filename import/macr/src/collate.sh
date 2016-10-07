#!bin/bash

PREFIX=ARRPT
TARGET_FILE=macr.csv

SORTED_YEARS=$(ls -1 input/${PREFIX}*LNG.TXT | sed -E 's/.*([0-9]{2}).*/\1/' | \
  awk ' { if ($1 > 79) { print 19$1 } else { print 20$1 }}' | sort | \
  sed -E 's/[0-9]{2}([0-9]{2})/\1/')

HEADER_DONE=0

rm -f ${TARGET_FILE}
touch ${TARGET_FILE}

for YEAR in $SORTED_YEARS; do
  FW_FILE=input/${PREFIX}${YEAR}LNG.TXT
  CSV_FILE=${PREFIX}${YEAR}LNG.csv
  echo "Converting ${FW_FILE} to csv"
  bin/tableDump "${FW_FILE}" "${CSV_FILE}"
  if [[ $HEADER_DONE -eq 0 ]]; then
    head -n 1 "${CSV_FILE}" > "${TARGET_FILE}"
    HEADER_DONE=1
  fi
  tail -n +2 "${CSV_FILE}" >> "${TARGET_FILE}"
  rm "${CSV_FILE}"
done

exit 0
