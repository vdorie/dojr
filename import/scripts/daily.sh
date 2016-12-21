# Placeholder file containing python jobs to run

SCRIPT_PATH=${HOME}/src/data-oasis/scripts

# Example for inserting new data into the macr table. Commented out for now.
# python3 ${SCRIPT_PATH}/update_macr_table.py ${HOME}/datakind/macr_2015.csv
python3 ${SCRIPT_PATH}/update_arrest_rates.py /Users/ng/Downloads/demo/data/arrest_rates.csv 2014
