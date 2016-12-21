class Table(object):
    TABLENAME = None
    # should be a list of ColumnDefinition objects
    COLUMNS = None

    @classmethod
    def header(cls):
        return [column.name for column in cls.COLUMNS]


class ColumnDefinition(object):
    """Data structure for setting column names and types in a database"""
    def __init__(self, column_name, column_type, col0=-1, col1=-1):
        """
        Args:
            column_name: name for the column (i.e., a string)
            column_type: data type for column (e.g., FLOAT, TEXT, etc)
        """

        self.name = column_name
        self.column_type = column_type
        self.col0 = col0
        self.col1 = col0 if col0>=1 and col1<0 else col1

    def __str__(self):
        return '{} {} {}-{}'.format(self.name, self.column_type, self.col0, self.col1)

    def __iter__(self):
        return iter((self.name, self.column_type))

    def wildcard(self):
        return {"INT":"%s", "TEXT":"%s", "FLOAT":"%s"}[self.column_type]

    def extract(self, line):
        str = line[self.col0-1:self.col1]
        # print self.name, line, str, self.column_type
        try:
            if self.column_type=="TEXT": return str
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


class CountyAgencyContext(object):
    """Table containing CA County contextual economic indicators."""
    TABLENAME = 'county_agency_context'
    COLUMNS = [
        ColumnDefinition('year', 'INT'),
        ColumnDefinition('county', 'TEXT'),
        ColumnDefinition('agency_name', 'TEXT'),
        ColumnDefinition('ncic_jurisdiction', 'TEXT'),
        ColumnDefinition('less_than_high_school', 'FLOAT'),
        ColumnDefinition('high_school_or_higher', 'FLOAT'),
        ColumnDefinition('bachelors_or_higher', 'FLOAT'),
        ColumnDefinition('per_capita_income', 'FLOAT'),
        ColumnDefinition('median_income', 'FLOAT'),
        ColumnDefinition('poverty_rate', 'FLOAT'),
        ColumnDefinition('employment_rate', 'FLOAT'),
        ColumnDefinition('unemployment_rate', 'FLOAT'),
    ]


class LeoAssaultsSummary(object):
    """Table containing CA LEO assaults summary data. Would be better were we to create this from an
    event level dataset."""
    TABLENAME = 'leo_assaults_summary'
    COLUMNS = [
        ColumnDefinition('year', 'INT'),
        ColumnDefinition('county', 'TEXT'),
        ColumnDefinition('injury_status', 'TEXT'),
        ColumnDefinition('assaults', 'INT'),
        ColumnDefinition('population', 'INT'),
        ColumnDefinition('assaults_per_per_100000', 'FLOAT'),
    ]


class CountyClearancesSummary(object):
    """Table containing CA country summary data for property and violent crimes."""
    TABLENAME = 'county_clearances_summary'
    COLUMNS = [
        ColumnDefinition('year', 'INT'),
        ColumnDefinition('county', 'TEXT'),
        ColumnDefinition('property_crimes', 'INT'),
        ColumnDefinition('violent_crimes', 'INT'),
        ColumnDefinition('population', 'INT'),
        ColumnDefinition('violent_crimes_per_100000', 'FLOAT'),
        ColumnDefinition('property_crimes_per_100000', 'FLOAT'),
        ColumnDefinition('property_clearance_rate', 'FLOAT'),
        ColumnDefinition('violent_clearance_rate', 'FLOAT'),
    ]


class CrimesClearances(object):
    """Table containing crimes and clearances data from 1982 to present."""
    TABLENAME = 'crimes_clearances'
    COLUMNS = [
        ColumnDefinition('ncic_code', 'TEXT'),
        ColumnDefinition('bcs_code', 'TEXT'),
        ColumnDefinition('year', 'INT'),
        ColumnDefinition('month', 'INT'),
        ColumnDefinition('crime_type', 'TEXT'),
        ColumnDefinition('amount', 'FLOAT'),
    ]


class Macr(object):
    """Table containing monthly arrests and citation records data."""
    TABLENAME = 'macr'
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
    ]


class CountyAgencyCode(object):
    """Table containing monthly arrests and citation records data."""
    TABLENAME = 'county_agency_code'
    COLUMNS = [
        ColumnDefinition('county_code', 'INT'),
        ColumnDefinition('county', 'TEXT'),
        ColumnDefinition('ncic_jurisdiction', 'TEXT'),
        ColumnDefinition('agency_name', 'TEXT'),
    ]


class CountyDemographics(object):
    """Table containing monthly arrests and citation records data."""
    TABLENAME = 'county_demographics'
    COLUMNS = [
        ColumnDefinition('year', 'INT'),
        ColumnDefinition('county', 'TEXT'),
        ColumnDefinition('race', 'TEXT'),
        ColumnDefinition('gender', 'TEXT'),
        ColumnDefinition('age_group', 'TEXT'),
        ColumnDefinition('population', 'INT'),
    ]


class OffenseSummaryCode(object):
    """Table containing BCS offense codes, summary codes, category, and human readable names."""
    TABLENAME = 'offense_summary_code'
    COLUMNS = [
        ColumnDefinition('bcs_offense_code', 'TEXT'),
        ColumnDefinition('bcs_summary_offense_code', 'TEXT'),
        ColumnDefinition('summary_offense_type', 'TEXT'),
        ColumnDefinition('offense_category', 'TEXT'),
        ColumnDefinition('before_2013', 'BOOLEAN'),
    ]
