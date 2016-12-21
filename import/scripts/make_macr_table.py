import sys
sys.path.append("C:\Users\GianneE\Desktop\datakind")

import argparse
import glob
import logging
import os
import sys
import time
# import urllib.parse
import urlparse

from data_oasis import tables
from data_oasis import utils

# note that there are two s's in postgress below
URL = urlparse.urlparse("postgress://postgres:duC71Hemi@localhost:5432/postgres")#urllib.parse.urlparse(os.environ['DATABASE_URL'])
logger = logging.getLogger()


def main():
    utils.setup_logging()
    parser = argparse.ArgumentParser()
    parser.add_argument('inpath', help='full path to directory containing MACR data')
    args = parser.parse_args()
    files = glob.glob('{}/macr*csv'.format(args.inpath))
    files.sort()
    num_files = len(files)
    with utils.get_connection(URL) as conn:
        table = tables.Macr()
        utils.drop_table(conn, table.TABLENAME)
        utils.create_table(conn, table.TABLENAME, table.COLUMNS)
        start = time.time()
        for num, f in enumerate(files):
            logger.info('Loading file %s of %s', num + 1, num_files)
            utils.load_data_from_file(conn, table.TABLENAME, f)
            conn.commit()
        end = time.time()
        logging.info('Completed loading %s files in %s seconds', num_files, end - start)


if __name__ == '__main__':
    sys.exit(main())
