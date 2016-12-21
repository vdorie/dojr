"""Script to clean up oddities in the macr dataset."""
import argparse
import glob
import logging
import sys

import pandas as pd


logging.basicConfig(stream=sys.stdout, level=logging.INFO)
logger = logging.getLogger()

# Replace true NaNs with NULL and change 'not applicable' to NaN. Also instances where there are
# blank strings that should actually be null values.
NULL_VAL = [pd.np.nan, 'not applicable', '   ', '  ']
SENTINEL_VAL = ['NULL', pd.np.nan, 'NULL', 'NULL']


# Pandas has trouble inferring data types for these columns, so manually enforce the type.
dtype = {
    'bcs_jurisdiction': str,
    'ncic_jurisdiction': str,
    'bcs_offense_code': str,
    'bcs_summary_offence_code': str,
    'fbi_offense_code': str,
}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('inpath', help='full path to directory containing MACR data')
    args = parser.parse_args()
    files = glob.glob('{}/macr*csv'.format(args.inpath))
    for f in files:
        logger.info('Loading file {}'.format(f))
        df = pd.read_csv(f, dtype=dtype)
        df = df.replace(NULL_VAL, SENTINEL_VAL)
        df = df.dropna()
        df.to_csv(f, index=False)


if __name__ == '__main__':
    sys.exit(main())
