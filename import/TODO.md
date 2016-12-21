* add documentation for setup
* ENV variables for credentials
* settle on schema
* setup read-only account for end users
* event-level data (right now, we only have year/month granularity)
* county clearance data X per 100000 metrics are strings containing commas and periods (ask Sundeep
  about this)
* automate deployment
* resolve VPC issue with AWS
* investigate airflow as a replacement for shell-scripts used here.
* null handling
* indexes and whatnot for tables
* possibly split stuff like year/county/population into distinct tables
* Add upserting/deduping
* Add function to fill missing data with 'NULL'
