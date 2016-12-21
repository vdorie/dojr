 SELECT t1.year,
    t2.county,
    t1.ncic_jurisdiction,
    t1.offense_level,
    t1.bcs_offense_code,
    t1.bcs_summary_offense_code,
    t1.status_type,
    t1.race,
    t1.total
   FROM (( SELECT macr.arrest_year AS year,
            macr.ncic_jurisdiction,
            macr.offense_level,
            macr.bcs_offense_code,
            macr.bcs_summary_offense_code,
            macr.status_type,
            macr.race_or_ethnicity AS race,
            count(*) AS total
           FROM macr
          GROUP BY macr.arrest_year, macr.ncic_jurisdiction, macr.offense_level, macr.bcs_offense_code, macr.race_or_ethnicity, macr.bcs_summary_offense_code, macr.status_type
          ORDER BY macr.arrest_year, macr.ncic_jurisdiction, macr.offense_level, macr.bcs_offense_code, macr.race_or_ethnicity, macr.bcs_summary_offense_code, macr.status_type) t1
     LEFT JOIN ( SELECT county_agency_code.ncic_jurisdiction,
            county_agency_code.county
           FROM county_agency_code) t2 ON ((t1.ncic_jurisdiction = t2.ncic_jurisdiction)))
  ORDER BY t1.year, t2.county, t1.ncic_jurisdiction, t1.race;