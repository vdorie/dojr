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

URL = urlparse.urlparse("postgress://postgres:duC71Hemi@localhost:5432/postgres")#urllib.parse.urlparse(os.environ['DATABASE_URL'])
logger = logging.getLogger()


def main():
    utils.setup_logging()
    parser = argparse.ArgumentParser()
    parser.add_argument('inpath', help='GoogleDrive/Team Shared folder - California OpenJustice/Data/arrests/macr/transformed_table_format')
    args = parser.parse_args()
    with utils.get_connection(URL) as conn:
        table = tables.Macr()
        utils.load_data_from_file(conn, table, args.inpath)


if __name__ == '__main__':
    sys.exit(main())
