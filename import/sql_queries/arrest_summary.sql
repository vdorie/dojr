 SELECT t1.year,
    t1.ncic_jurisdiction,
    t1.county,
    t1.offense_level,
    t1.bcs_offense_code,
    t1.bcs_summary_offense_code,
    t1.status_type,
    t1.race,
    t1.gender,
    t1.age_group,
    t1.arrests
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text AS race,
            int_arrest_summary.gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.gender, int_arrest_summary.age_group) t1
UNION
 SELECT t2.year,
    t2.ncic_jurisdiction,
    t2.county,
    t2.offense_level,
    t2.bcs_offense_code,
    t2.bcs_summary_offense_code,
    t2.status_type,
    t2.race,
    t2.gender,
    t2.age_group,
    t2.arrests
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text AS race,
            'All Combined'::text AS gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.age_group) t2
UNION
 SELECT t2a.year,
    t2a.ncic_jurisdiction,
    t2a.county,
    t2a.offense_level,
    t2a.bcs_offense_code,
    t2a.bcs_summary_offense_code,
    t2a.status_type,
    t2a.race,
    t2a.gender,
    t2a.age_group,
    t2a.arrests
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            int_arrest_summary.gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race, int_arrest_summary.gender, int_arrest_summary.age_group) t2a
UNION
 SELECT t2b.year,
    t2b.ncic_jurisdiction,
    t2b.county,
    t2b.offense_level,
    t2b.bcs_offense_code,
    t2b.bcs_summary_offense_code,
    t2b.status_type,
    t2b.race,
    t2b.gender,
    t2b.age_group,
    t2b.arrests
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text as race,
            int_arrest_summary.gender,
            'All Combined'::text as age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.gender) t2b
UNION
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
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text AS race,
            'All Combined'::text AS gender,
            'All Combined'::text AS age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type) t3
UNION
 SELECT t4.year,
    t4.ncic_jurisdiction,
    t4.county,
    t4.offense_level,
    t4.bcs_offense_code,
    t4.bcs_summary_offense_code,
    t4.status_type,
    t4.race,
    t4.gender,
    t4.age_group,
    t4.arrests
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            'All Combined'::text AS gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race, int_arrest_summary.age_group) t4
UNION
 SELECT t5.year,
    t5.ncic_jurisdiction,
    t5.county,
    t5.offense_level,
    t5.bcs_offense_code,
    t5.bcs_summary_offense_code,
    t5.status_type,
    t5.race,
    t5.gender,
    t5.age_group,
    t5.arrests
   FROM (SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            'All Combined'::text AS gender,
            'All Combined'::text AS age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race) t5
UNION
 SELECT t6.year,
    t6.ncic_jurisdiction,
    t6.county,
    t6.offense_level,
    t6.bcs_offense_code,
    t6.bcs_summary_offense_code,
    t6.status_type,
    t6.race,
    t6.gender,
    t6.age_group,
    t6.arrests
   FROM ( SELECT int_arrest_summary.year,
            int_arrest_summary.ncic_jurisdiction,
            int_arrest_summary.county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            int_arrest_summary.gender,
            'All Combined'::text AS age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.ncic_jurisdiction, int_arrest_summary.county, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race, int_arrest_summary.gender) t6
UNION
 SELECT t7.year,
    t7.ncic_jurisdiction,
    t7.county,
    t7.offense_level,
    t7.bcs_offense_code,
    t7.bcs_summary_offense_code,
    t7.status_type,
    t7.race,
    t7.gender,
    t7.age_group,
    t7.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text AS race,
            int_arrest_summary.gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.gender, int_arrest_summary.age_group) t7
UNION
 SELECT t8.year,
    t8.ncic_jurisdiction,
    t8.county,
    t8.offense_level,
    t8.bcs_offense_code,
    t8.bcs_summary_offense_code,
    t8.status_type,
    t8.race,
    t8.gender,
    t8.age_group,
    t8.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text AS race,
            'All Combined'::text AS gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.age_group) t8
UNION
 SELECT t9.year,
    t9.ncic_jurisdiction,
    t9.county,
    t9.offense_level,
    t9.bcs_offense_code,
    t9.bcs_summary_offense_code,
    t9.status_type,
    t9.race,
    t9.gender,
    t9.age_group,
    t9.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text AS race,
            'All Combined'::text AS gender,
            'All Combined'::text AS age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type) t9
UNION
 SELECT t10.year,
    t10.ncic_jurisdiction,
    t10.county,
    t10.offense_level,
    t10.bcs_offense_code,
    t10.bcs_summary_offense_code,
    t10.status_type,
    t10.race,
    t10.gender,
    t10.age_group,
    t10.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            'All Combined'::text AS gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race, int_arrest_summary.age_group) t10
UNION
 SELECT t11.year,
    t11.ncic_jurisdiction,
    t11.county,
    t11.offense_level,
    t11.bcs_offense_code,
    t11.bcs_summary_offense_code,
    t11.status_type,
    t11.race,
    t11.gender,
    t11.age_group,
    t11.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            'All Combined'::text AS gender,
            'All Combined'::text AS age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race) t11
UNION
 SELECT t12.year,
    t12.ncic_jurisdiction,
    t12.county,
    t12.offense_level,
    t12.bcs_offense_code,
    t12.bcs_summary_offense_code,
    t12.status_type,
    t12.race,
    t12.gender,
    t12.age_group,
    t12.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            int_arrest_summary.gender,
            'All Combined'::text AS age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race, int_arrest_summary.gender) t12
UNION
 SELECT t12a.year,
    t12a.ncic_jurisdiction,
    t12a.county,
    t12a.offense_level,
    t12a.bcs_offense_code,
    t12a.bcs_summary_offense_code,
    t12a.status_type,
    t12a.race,
    t12a.gender,
    t12a.age_group,
    t12a.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            int_arrest_summary.race,
            int_arrest_summary.gender,
            int_arrest_summary.age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.race, int_arrest_summary.gender, int_arrest_summary.age_group) t12a
UNION
 SELECT t12b.year,
    t12b.ncic_jurisdiction,
    t12b.county,
    t12b.offense_level,
    t12b.bcs_offense_code,
    t12b.bcs_summary_offense_code,
    t12b.status_type,
    t12b.race,
    t12b.gender,
    t12b.age_group,
    t12b.arrests
   FROM ( SELECT int_arrest_summary.year,
            'All Combined'::text AS ncic_jurisdiction,
            'All Combined'::text AS county,
            int_arrest_summary.offense_level,
            int_arrest_summary.bcs_offense_code,
            int_arrest_summary.bcs_summary_offense_code,
            int_arrest_summary.status_type,
            'All Combined'::text as race,
            int_arrest_summary.gender,
            'All Combined'::text as age_group,
            sum(int_arrest_summary.arrests) AS arrests
           FROM int_arrest_summary
          GROUP BY int_arrest_summary.year, int_arrest_summary.offense_level, int_arrest_summary.bcs_offense_code, int_arrest_summary.bcs_summary_offense_code, int_arrest_summary.status_type, int_arrest_summary.gender) t12b
  ORDER BY 1, 2, 5, 8, 9, 10;
