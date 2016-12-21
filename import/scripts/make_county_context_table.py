import argparse
import os
import sys
import urllib.parse

from data_oasis import tables
from data_oasis import utils

URL = urllib.parse.urlparse(os.environ['DATABASE_URL'])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('infile', help='full path to file containing county contextual data')
    args = parser.parse_args()
    with utils.get_connection(URL) as conn:
        table = tables.CountyAgencyContext()
        utils.recreate_table_from_file(conn, table, args.infile)


if __name__ == '__main__':
    sys.exit(main())
