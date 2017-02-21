load(file.path("..", "common", "data", "obts_clean.Rdata"))

## eliminate PII
obts$age_at_arrest <-
  with(obts, arrest_event_year - birth_year +
         ifelse(arrest_event_month > birth_month | 
                (arrest_event_month == birth_month & arrest_event_day >= birth_day) |
                is.na(birth_month), 0L, -1L))
obts$birth_date <- NULL
obts$court_number <- NULL
obts$local_file_number <- NULL
obts$name <- NULL
obts$cii_number <- NULL
obts$birth_year <- NULL
obts$birth_month <- NULL
obts$birth_day <- NULL

## redefine to use in join on qualifiers
obts$id <- seq_len(nrow(obts))

arrestQualifiers <- lapply(as.character(obts$arrest_qualifier), function(qualifier) {
  qualifiers <- substring(qualifier, seq.int(1L, 13L, 2L), seq.int(2L, 14L, 2L))
  qualifiers[qualifiers != "  "]
})

arrestQualifiers <- data.frame(id = rep.int(obts$id, sapply(arrestQualifiers, length)),
                               qualifier = as.factor(unlist(arrestQualifiers)))
invisible(gc(FALSE))

courtQualifiers <- lapply(as.character(obts$court_qualifier), function(qualifier) {
  qualifiers <- substring(qualifier, seq.int(1L, 13L, 2L), seq.int(2L, 14L, 2L))
  qualifiers[qualifiers != "  "]
})

courtQualifiers <- data.frame(id = rep.int(obts$id, sapply(courtQualifiers, length)),
                              qualifier = as.factor(unlist(courtQualifiers)))

obts$arrest_qualifier <- NULL
obts$court_qualifier <- NULL

## drop as redundant
obts$court_event_date <- NULL
obts$arrest_event_date <- NULL
obts$arrest_disposition_date <- NULL

write.csv(obts, file = "obts.csv", row.names = FALSE)
write.csv(arrestQualifiers, file = "arrest_qualifiers.csv", row.names = FALSE)
write.csv(courtQualifiers, file = "court_qualifiers.csv", row.names = FALSE)

file.copy(file.path("..", "common", "data", "cjis_codes.csv"), "cjis_codes.csv")
file.copy(file.path("..", "common", "data", "judicial_districts.csv"), "judicial_districts.csv")
file.copy(file.path("..", "common", "data", "arresting_agencies.csv"), "arresting_agencies.csv")
file.copy(file.path("..", "common", "data", "bcs_offense_codes.csv"), "bcs_offense_codes.csv")
file.copy(file.path("..", "common", "data", "qualifier_codes.csv"), "qualifier_codes.csv")

writeLines(
"## Files included:

  * obts.csv - main file
  * arrest_qualifiers.csv - table of arrest qualifiers used in rows of obts.csv; join on `id`
  * court_qualifiers.csv - table of court qualifiers used in rows of obts.csv; join on `id`
  * qualifier_codes.csv - map of qualifier code abbreviations to descriptions
  * cjis_codes.csv - codes matching obts columns `arrest_offense` and `court_disposition_offense`
  * bcs_offense_codes.csv - codes matching columns `arrest_summary_code` and `court_summary_code`; more concise version of cjis_codes
  * arresting_agencies.csv - codes matching column `arresting_agency`
  * judicial_districts.csv - codes matching column `court_judicial_district`",
"readme.Md")
