#!bin/bash

PREFIX=ARMST
TARGET_FILE=macr_pii.csv
TARGET_FILE_NAME=macr_pii_name.txt

SORTED_YEARS=$(ls -1 input/${PREFIX}*LNG.TXT | sed -E 's/.*([0-9]{2}).*/\1/' | \
  awk ' { if ($1 > 79) { print 19$1 } else { print 20$1 }}' | sort | \
  sed -E 's/[0-9]{2}([0-9]{2})/\1/')

HEADER_DONE=0

rm -f ${TARGET_FILE} ${TARGET_FILE_NAME}
touch ${TARGET_FILE}
touch ${TARGET_FILE_NAME}

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
  
  CSV_FILE=${PREFIX}${YEAR}LNG_name.csv
  bin/tableDump_name "${FW_FILE}" "${CSV_FILE}"
  tail -n +2 "${CSV_FILE}" >> "${TARGET_FILE_NAME}"
  rm "${CSV_FILE}"
done

exit 0
