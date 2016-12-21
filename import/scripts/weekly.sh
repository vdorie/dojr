# Placeholder file containing python jobs to run

SCRIPT_PATH=${HOME}/src/data-oasis/scripts

python3 ${SCRIPT_PATH}/make_county_agency_codes.py ${HOME}/datakind/ca_law_enforcement_agency_codes.csv
python3 ${SCRIPT_PATH}/make_county_clearance_summary_table.py ${HOME}/datakind/ca_doj_county_crimes_clearances_summary_2005-2014_02-17-2016.csv
python3 ${SCRIPT_PATH}/make_county_context_table.py ${HOME}/datakind/ca_county_agency_contextual_indicators_2009-2014_04-25-2016.csv
python3 ${SCRIPT_PATH}/make_county_demographics_table.py ${HOME}/datakind/ca_county_population_by_race_gender_age_1980-2015_08-17-2016.csv
python3 ${SCRIPT_PATH}/make_crimes_clearances_table.py ${HOME}/datakind/ca_county_population_by_race_gender_age_1980-2015_08-17-2016.csv
python3 ${SCRIPT_PATH}/make_leo_assaults_summary_table.py ${HOME}/datakind/ca_doj_county_law_enforcement_officer_assaults_summary_2005-2014_02-17-2016.csv
python3 ${SCRIPT_PATH}/make_offense_summary_code_table.py ${HOME}/datakind/ca_doj_bcs_offense_codes_combined.csv
# Example for inserting new data into the macr table. Commented out for now.
# python3 ${SCRIPT_PATH}/update_macr_table.py ${HOME}/datakind/macr_2015.csv

# NOTE: 04_03_2016 is a directory containing MACR csv files. Commented out for now.
# python3 ${SCRIPT_PATH}/make_macr_table.py ${HOME}/datakind/04_03_2016
