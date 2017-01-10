import argparse
import getpass
import glob
import os.path
import psycopg2
import urlparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('inpath', help='GoogleDrive/Team Shared folder - California OpenJustice/Data/arrests/macr/transformed_table_format')
    args = parser.parse_args()

    passwd = getpass.getpass()

    url = urlparse.urlparse("postgress://postgres@hdcvmappsvr32.rescs.caldoj.local:5432/postgres")

    with psycopg2.connect(database=url.path[1:], user=url.username,
        password=passwd, host=url.hostname, port=url.port) as conn:
        create_table(conn, Macr())
        load_data_from_file(conn, Macr(), args.inpath)

def load_data_from_file(conn, table, infile):
    if os.path.isdir(infile):
       paths = glob.glob(infile+'/*TXT')
    else:
        paths = [infile]

    for path in paths:
    	filename = os.path.basename(path)
        print 'processing ', filename

        nlines = []
        with open(path, 'r') as f:
            nlines = sum(1 for line in f)
        print '    %d records to import'%nlines

        i = 1
        numinserted = 0
        with conn.cursor() as cur:
            fieldnames = ",".join([c.name for c in table.COLUMNS])
            wildcards = ",".join([c.wildcard() for c in table.COLUMNS])
            query = "INSERT INTO {tablename} ({fieldnames}) VALUES ({wildcards})".format(
                tablename=table.TABLENAME, fieldnames=fieldnames, wildcards=wildcards)

            # might be preferable to do WHERE NOT EXISTS (SELECT ...) since ON CONFLICT is non-standard
            query += " ON CONFLICT DO NOTHING"
            with open(path, 'r') as f:
                rows = []
                for line in f:
                    row = []
                    for coldef in table.COLUMNS:
                        val = coldef.extract(line, filename, i)
                        val = noArrestBlankReplacement(coldef.name, val)
                        row.append(val)
                    rows.append(row)
                    if len(rows)==10000:
                        cur.executemany(query, rows)
                        conn.commit()
                        numinserted += cur.rowcount
                        rows = []
                    if i==1 or i%100000==0 or i==nlines:
                        print "    imported %7d records -- %5.1f%%"%(i, i*100./nlines)
                    i += 1
                if len(rows)>0:
                    cur.executemany(query, rows)
                    conn.commit()
                    numinserted += cur.rowcount
        print "    %d inserted, %d ignored as duplicate"%(numinserted, nlines - numinserted)

def noArrestBlankReplacement(name, str):
	# we found instances of character 255 in some record type 91 rows,
	# which are records for events in which no arrest occurred.
	# Note this is a problem because the database is UTF-8 encoded,
	# and files as far we know are ASCII. Characters in the range 129-255
	# can cause the file to not be a valid UTF-8 encoding, making the insert
	# crash. For the most part, ASCII input is also valid UTF-8, so we
	# are only narrowly replacing the one place where we found a problem.
    if name not in ['fbi_offense_code', 'bcs_summary_offense_code']:
        return str
    if chr(255) not in str:
        return str
    return ''.join([' ' if c == chr(255) else c for c in str])


def create_table(conn, table):
    """Params:
        conn: a connection to Postgres
        table: a Table object
    """
    with conn.cursor() as cur:
        create = """CREATE TABLE IF NOT EXISTS {} ({})""".format(
            table.TABLENAME, ','.join([coldef.sqlDef() for coldef in table.COLUMNS]))
        cur.execute(create)
    conn.commit()

class TableDef(object):
    TABLENAME = None
    # should be a list of ColumnDefinition objects
    COLUMNS = None

    @classmethod
    def header(cls):
        return [column.name for column in cls.COLUMNS]

class ColumnDefinition(object):
    """Data structure for setting column names and types in a database"""
    def __init__(self, column_name, column_type, col0=-1, col1=-1, column_constraint=None):
        """
        Args:
            column_name: name for the column (i.e., a string)
            column_type: data type for column (e.g., FLOAT, TEXT, etc)
        """
        self.name = column_name
        self.column_type = column_type
        self.col0 = col0
        self.col1 = col0 if col0>=1 and col1<0 else col1
        self.column_constraint = column_constraint

    def __str__(self):
        return '{} {} {}-{}'.format(self.name, self.column_type, self.col0, self.col1)

    def __iter__(self):
        return iter((self.name, self.column_type))

    def sqlDef(self):
        if self.column_constraint:
            return '{} {} {}'.format(self.name, self.column_type, self.column_constraint)
        else:
            return '{} {}'.format(self.name, self.column_type)

    def wildcard(self):
        return {"INT":"%s", "TEXT":"%s", "FLOAT":"%s"}[self.column_type]

    def extract(self, line, filename, recno):
    	# bunch-o-hacks to give us the value to put in the database
        if self.name=="internal_uid":
        	return "%s:%d"%(filename, recno)

        str = line[self.col0-1:self.col1]
        try:
            if   self.column_type=="TEXT": return str
            elif self.column_type=="INT": return int(str)
            elif self.column_type=="FLOAT": return float(str)
            else: raise Exception("d'oh! type="+self.column_type)
        except:
            if str.strip()=="":
                return None
            print self
            print "\tline=",line.strip()
            print "\tstr=",str
            raise

class Macr(TableDef):
    """Monthly Arrest and Citation Record (MACR) without PII."""
    TABLENAME = 'macr2'
    COLUMNS = [
        ColumnDefinition('record_type_id', 'INT', 1, 2),
        ColumnDefinition('bcs_jurisdiction', 'TEXT', 3, 7),
        ColumnDefinition('ncic_jurisdiction', 'TEXT', 8, 11),
        ColumnDefinition('arrest_year', 'INT', 14, 17),
        ColumnDefinition('arrest_month', 'INT', 18, 19),
        ColumnDefinition('arrest_day', 'INT', 20, 21),
        ColumnDefinition('summary_offense_level', 'TEXT', 22),
        ColumnDefinition('offense_level', 'TEXT', 23),
        ColumnDefinition('bcs_offense_code', 'TEXT', 24, 26),
        ColumnDefinition('bcs_summary_offense_code', 'TEXT', 27, 28),
        ColumnDefinition('fbi_offense_code', 'TEXT', 30, 32),
        ColumnDefinition('age', 'FLOAT', 78, 80),
        ColumnDefinition('race_or_ethnicity', 'TEXT', 81),
        ColumnDefinition('gender', 'TEXT', 82),
        ColumnDefinition('status_type', 'TEXT', 83),
        ColumnDefinition('disposition', 'TEXT', 84),
        ColumnDefinition('internal_uid', 'TEXT', column_constraint='UNIQUE'),
    ]

if __name__ == '__main__':
    main()

