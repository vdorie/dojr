## temporarily using ojdr version since it is cleaner; in the long-run
## need to transition all files to use that but that can only happen
## after the ojdr is fully implemented
load(file.path("..", "ojdr", "datasets", "macr", "macr_clean.Rdata"))



## eliminate PII
macr$name <- NULL
macr$reference_number <- NULL
macr$birth_year <- NULL
macr$birth_month <- NULL
macr$birth_day <- NULL

colnames(macr)[colnames(macr) == "record_id"] <- "record_type"

invisible(gc(FALSE))

#write.csv(obts, file = "obts.csv", row.names = FALSE)
require(readstata13)
save.dta13(macr, file = "macr.dta")

file.copy(file.path("..", "common", "data", "bcs_offense_codes.csv"), "bcs_offense_codes.csv")

jurisdictions <- read.csv(file.path("..", "common", "data", "jurisdictions.csv"))
colnames(jurisdictions) <- c("county_code", "county", "ncic_code", "name", "start_date", "end_date", "contract", "CJSC_notes", "bcs_code")
sub("(.*)(?:County)(.*)", "\\1\\2", "Alameda County", perl = TRUE)

levels(jurisdiction$county) <- trimws(sub("(.*)(?:County)(.*)", "\\1\\2", jurisdictions$county, perl = TRUE))

write.csv(jurisdictions, file = "jurisdictions.csv", row.names = FALSE)

writeLines(
"## Files included:

  * macr.dta - main file
  * bcs\\_offense\\_codes.csv - codes matching columns `bcs_offense_code` and `bcs_summary_offense_code`; column `new_2013` means that the code was introduced into usage in 2013
  * jurisdictions.csv - codes matching column `ncic_jurisdiction` (preferred) and `bcs_jurisdiction` (deprecated)",
"readme.Md")
