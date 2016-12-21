from future.standard_library import install_aliases
install_aliases()

import argparse
import logging
import os
import sys
import urllib.parse

import pandas as pd

from data_oasis import utils

URL = urllib.parse.urlparse(os.environ['DATABASE_URL'])
logger = logging.getLogger()


def main():
    utils.setup_logging()
    parser = argparse.ArgumentParser()
    parser.add_argument('output_path', help='full path to directory containing MACR data')
    parser.add_argument('year', help='full path to directory containing MACR data')

    args = parser.parse_args()

    # query to get arrest rates, broken down by race and year.
    query = """
        WITH
            demographics AS (
                SELECT year, race, population
                FROM year_county_race
                WHERE county = 'All Combined' AND race <> 'All Combined'
                ),

            crime_by_race_total AS (
                SELECT year, race, sum(total) AS num_crimes
                FROM year_jurisdiction_offense
                where year > 2004 AND year < {}
                GROUP BY year, race
            )

        SELECT d.year, d.race, cd.num_crimes / d.population as arrest_rate
        FROM demographics as d
        JOIN crime_by_race_total as cd
        ON d.year = cd.year
        AND d.race = cd.race
        ORDER BY year, race;
        """.format(args.year)

    with utils.get_connection(URL).cursor() as cur:
        cur.execute(query)
        cols = [c[0] for c in cur.description]
        result = cur.fetchall()
        df = pd.DataFrame(result, columns=cols)
        df['arrest_rate'] = 100000 * df['arrest_rate'].astype(float)

    pivot = df.pivot('year', 'race', 'arrest_rate').T
    pivot.to_csv(args.output_path)

if __name__ == '__main__':
    sys.exit(main())
