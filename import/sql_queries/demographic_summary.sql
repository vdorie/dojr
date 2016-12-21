 SELECT t1.year,
    t1.county,
    t2.race,
    t2.gender,
    t2.age_group,
    t2.population,
    ((100.0 * (t2.population)::numeric) / (t1.county_population)::numeric) AS percentage
   FROM (( SELECT county_demographics.year,
            county_demographics.county,
            max(county_demographics.population) AS county_population
           FROM county_demographics
          WHERE ((county_demographics.gender = 'All Combined'::text) AND (county_demographics.age_group = 'All Combined'::text))
          GROUP BY county_demographics.year, county_demographics.county
          ORDER BY county_demographics.year, county_demographics.county) t1
     LEFT JOIN ( SELECT county_demographics.year,
            county_demographics.county,
            county_demographics.race,
            county_demographics.gender,
            county_demographics.age_group,
            county_demographics.population
           FROM county_demographics
          WHERE (county_demographics.age_group = ANY (ARRAY['All Combined'::text, 'Juvenile'::text, 'Adult'::text]))
          ORDER BY county_demographics.year, county_demographics.county) t2 ON (((t1.year = t2.year) AND (t1.county = t2.county))))
  ORDER BY t1.year, t1.county, t2.race, t2.gender, t2.age_group;