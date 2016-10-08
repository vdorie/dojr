dropBadRows <- FALSE

macr <- read.csv("macr.csv", fileEncoding = "latin1", sep = "|")

"%not_in%" <- function(x, table) match(x, table, nomatch = 0L) <= 0L

badRows <- macr$fbi_offense_code %in% c("\177\177\177", "\177", "ÿ")
if (dropBadRows) {
  macr <- macr[!badRows,]
  macr$fbi_offense_code[macr$fbi_offense_code == ""] <- NA
} else {
  macr$fbi_offense_code[macr$fbi_offense_code == "" | badRows] <- NA
}
macr$fbi_offense_code <- droplevels(macr$fbi_offense_code)

invisible(gc(FALSE))

macr$record_type_id    <- factor(macr$record_type_id)
macr$bcs_jurisdiction  <- factor(macr$bcs_jurisdiction)

macr$ncic_jurisdiction[macr$ncic_jurisdiction == ""] <- NA
macr$ncic_jurisdiction <- droplevels(macr$ncic_jurisdiction)

macr$summary_offense_level[macr$summary_offense_level == ""] <- NA
macr$summary_offense_level <- droplevels(macr$summary_offense_level)
levels(macr$summary_offense_level) <- c("felony", "juvenile", "misdemeanor")

macr$offense_level <- factor(macr$offense_level, labels = c("status offense", "misdemeanor", "felony"))

macr$bcs_offense_code <- factor(macr$bcs_offense_code)
macr$bcs_summary_offense_code <- factor(macr$bcs_summary_offense_code)

## have to do some annoying things to accomodate for the fact that prior to 1991,
## race was coded as 1-9, and after it was coded using A/B/C...
raceCategories.old <- c(
  "", "White", "Hispanic", "Black", "American Indian",             ## old codes (<1991)
  "Chinese", "Japanese", "Filipino", "Other", "Pacific Islander",
  "Other Asian", "Black", "Chinese", "Cambodian", "Filipino",      ## new codes
  "Guamanian", "Hispanic", "American Indian", "Japanese", "Korean",
  "Laotian", "Other", "Pacific Islander", "Samoan", "Hawaiian",
  "Vietnamese", "White", "Asian Indian")
raceCategories <- unique(raceCategories.old)
raceMap <- match(raceCategories.old, raceCategories)

macr$race_or_ethnicity <- factor(raceMap[macr$race_or_ethnicity], labels = raceCategories)
macr$race_or_ethnicity[macr$race_or_ethnicity == ""] <- NA
macr$race_or_ethnicity <- droplevels(macr$race_or_ethnicity)

macr$gender <- factor(macr$gender, labels = c("male", "female"))
macr$status_type <- factor(macr$status_type, labels = c("cited", "booked", "other"))
macr$disposition <- factor(macr$disposition, labels = c(
  "released", "turned over to other agency", "misdemeanor complaint sought",
  "felony complaint sought", "referred to juvenile probation department", "handled within department"))

invisible(gc(FALSE))

save(macr, file = "macr.Rdata")
