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

names(obts)[names(obts) == "db_id"] <- "db_id"

arrestQualifiers <- lapply(as.character(obts$arrest_qualifier), function(qualifier) {
  qualifiers <- substring(qualifier, seq.int(1L, 13L, 2L), seq.int(2L, 14L, 2L))
  qualifiers[qualifiers != "  "]
})

arrestQualifiers <- data.frame(db_id = rep.int(obts$db_id, sapply(arrestQualifiers, length)),
                               qualifier = as.factor(unlist(arrestQualifiers)))

courtQualifiers <- lapply(as.character(obts$court_qualifier), function(qualifier) {
  qualifiers <- substring(qualifier, seq.int(1L, 13L, 2L), seq.int(2L, 14L, 2L))
  qualifiers[qualifiers != "  "]
})

courtQualifiers <- data.frame(db_id = rep.int(obts$db_id, sapply(courtQualifiers, length)),
                              qualifier = as.factor(unlist(courtQualifiers)))

obts$arrest_qualifier <- NULL
obts$court_qualifier <- NULL

invisible(gc(FALSE))

## change to days between arrest_disposition and whatever
obts$days_to_arrest_disposition <-
  with(obts, as.integer(difftime(arrest_disposition_date, arrest_event_date, units = "days")))
obts$days_to_court_disposition <- 
  with(obts, as.integer(difftime(court_event_date, arrest_event_date, units = "days")))

## drop as redundant
obts$court_event_date <- NULL
obts$arrest_event_date <- NULL
obts$arrest_disposition_date <- NULL

## drop as TMI
obts$arrest_event_day <- NULL
obts$court_event_day <- NULL
obts$court_event_month <- NULL
obts$court_event_year <- NULL
obts$arrest_disposition_day <- NULL
obts$arrest_disposition_month <- NULL
obts$arrest_disposition_year <- NULL

obts$arrest_bypass <- NULL
obts$cii_record_type <- NULL
obts$arrest_edit_error_code <- NULL
obts$arrest_converted_data <- NULL
obts$court_bypass <- NULL
obts$court_converted_data <- NULL
obts$court_edit_error_code <- NULL

levels(obts$pdr_id) <- c("false", "true", "unknown")
names(obts)[names(obts) == "pdr_id"] <- "new_offender"

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
