# Placeholder file containing python jobs to run

VENV_PATH=${HOME}/.virtualenvs/doj/bin
SCRIPT_PATH=${HOME}/src/data-oasis/scripts

source ${VENV_PATH}/activate

${VENV_PATH}/python3 ${SCRIPT_PATH}/make_county_context_table.py ${HOME}/datakind/ca_county_agency_contextual_indicators_2009-2014_04-25-2016.csv
${VENV_PATH}/python3 ${SCRIPT_PATH}/make_leo_assaults_summary_table.py ${HOME}/datakind/ca_doj_county_law_enforcement_officer_assaults_summary_2005-2014_02-17-2016.csv
# XXX: Original data from DOJ required munging the X_per_100000 rates with val = val.replace(',',
# '') so that the metrics could be cast as floats.
${VENV_PATH}/python3 ${SCRIPT_PATH}/make_county_clearance_summary_table.py ${HOME}/datakind/ca_doj_county_crimes_clearances_summary_2005-2014_02-17-2016.csv
