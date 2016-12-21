"""Script to add indexes to the macr table."""
import logging
import os
import sys
import time
import urllib.parse

from data_oasis import tables
from data_oasis import utils


URL = urllib.parse.urlparse(os.environ['DATABASE_URL'])
logger = logging.getLogger()


def main():
    utils.setup_logging()
    with utils.get_connection(URL) as conn:
        table = tables.Macr()
        start = time.time()
        idxname_columnname = (
            ('idx_macr_arrest_year', 'arrest_year'),
            ('idx_macr_arrest_month', 'arrest_month'),
            ('idx_macr_offense_level', 'offense_level'),
            ('idx_macr_bcs_summary_offense_code', 'bcs_summary_offense_code'),
            ('idx_macr_race_or_ethnicity', 'race_or_ethnicity'),
            ('idx_macr_status_type', 'status_type'),
            ('idx_macr_disposition', 'disposition'),
        )
        for idx in idxname_columnname:
            idxname, columnname = idx
            logger.info('Adding index %s to %s', idxname, columnname)
            utils.create_index(conn, table.TABLENAME, idxname, columnname)
            conn.commit()
        end = time.time()
        logging.info('Completed adding indexes in %s seconds', end - start)


if __name__ == '__main__':
    sys.exit(main())
