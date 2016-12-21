import argparse
import os
import sys
import urllib.parse

from data_oasis import tables
from data_oasis import utils

URL = urllib.parse.urlparse(os.environ['DATABASE_URL'])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('infile', help='GoogleDrive/Team Shared folder - California OpenJustice/Data/County Demographics')
    args = parser.parse_args()
    with utils.get_connection(URL) as conn:
        table = tables.CountyDemographics()
        utils.recreate_table_from_file(conn, table, args.infile)


if __name__ == '__main__':
    sys.exit(main())
