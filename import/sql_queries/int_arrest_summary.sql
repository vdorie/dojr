 SELECT t3.year,
    t3.ncic_jurisdiction,
    t3.county,
    t3.offense_level,
    t3.bcs_offense_code,
    t3.bcs_summary_offense_code,
    t3.status_type,
    t3.race,
    t3.gender,
    t3.age_group,
    t3.arrests
   FROM ( SELECT t1.year,
            t1.ncic_jurisdiction,
            t2.county,
            t1.offense_level,
            t1.bcs_offense_code,
            t1.bcs_summary_offense_code,
            t1.status_type,
            t1.race,
            t1.gender,
            t1.age_group,
            t1.arrests
           FROM (( SELECT t0.arrest_year AS year,
                    t0.ncic_jurisdiction,
                    t0.offense_level,
                    t0.bcs_offense_code,
                    t0.bcs_summary_offense_code,
                    t0.status_type,
                    t0.race_or_ethnicity AS race,
                    t0.gender,
                    t0.age_group,
                    count(*) AS arrests
                   FROM ( SELECT macr.record_type_id,
                            macr.bcs_jurisdiction,
                            macr.ncic_jurisdiction,
                            macr.arrest_year,
                            macr.arrest_month,
                            macr.arrest_day,
                            macr.summary_offense_level,
                            macr.offense_level,
                            macr.bcs_offense_code,
                            macr.bcs_summary_offense_code,
                            macr.fbi_offense_code,
                            macr.age,
                            macr.race_or_ethnicity,
                            macr.gender,
                            macr.status_type,
                            macr.disposition,
                                CASE
                                    WHEN (macr.age < (18)::double precision) THEN 'Juvenile'::text
                                    ELSE 'Adult'::text
                                END AS age_group
                           FROM macr) t0
                  GROUP BY t0.arrest_year, t0.ncic_jurisdiction, t0.offense_level, t0.bcs_offense_code, t0.race_or_ethnicity, t0.gender, t0.bcs_summary_offense_code, t0.status_type, t0.age_group
                  ORDER BY t0.arrest_year, t0.ncic_jurisdiction, t0.offense_level, t0.bcs_offense_code, t0.race_or_ethnicity, t0.gender, t0.bcs_summary_offense_code, t0.status_type, t0.age_group) t1
             LEFT JOIN ( SELECT county_agency_code.ncic_jurisdiction,
                    county_agency_code.county
                   FROM county_agency_code) t2 ON ((t1.ncic_jurisdiction = t2.ncic_jurisdiction)))
          ORDER BY t1.year, t2.county, t1.ncic_jurisdiction, t1.race, t1.gender) t3;