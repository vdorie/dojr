macr <- read.csv("macr_pii.csv", fileEncoding = "latin1", sep = "|")

macr$record_type_id    <- factor(macr$record_type_id)
macr$bcs_jurisdiction  <- factor(macr$bcs_jurisdiction)

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

macr$race <- factor(raceMap[macr$race], labels = raceCategories)
macr$race[macr$race == ""] <- NA
macr$race <- droplevels(macr$race)

macr$gender <- factor(macr$gender, labels = c("male", "female"))
macr$status_type <- factor(macr$status_type, labels = c("cited", "booked", "other"))
macr$disposition <- factor(macr$disposition, labels = c(
  "released", "turned over to other agency", "misdemeanor complaint sought",
  "felony complaint sought", "referred to juvenile probation department", "handled within department"))

macr$fbi_offense_code[macr$fbi_offense_code %in% c("", "\177\177\177", "\177", "ÿ")] <- NA
macr$fbi_offense_code <- droplevels(macr$fbi_offense_code)

invisible(gc(FALSE))

macr$name <- readLines("macr_pii_name.txt")

invisible(gc(FALSE))

save(macr, file = "macr_pii.Rdata")
