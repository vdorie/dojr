rawFile <- file.path("datasets", "jcpss", "raw", "jcpss_OJ.csv")

colNames <- readLines(rawFile, n = 1L)
colNames <- strsplit(colNames, ",")[[1L]]
colNames <- sub("\"(.*)\"", "\\1", colNames)

colClasses <- rep_len("factor", length(colNames))
## these have changed in the 2003-2016 file
#colClasses[colNames %in% c("REFERRAL.COUNTY.CODE", "AGE.AT.REFERRAL", "REPORT_YEAR")] <- "integer"
#colClasses[colNames %in% c("", "ACTION.DATE")] <- "character"
colClasses[colNames %in% c("", "ACTION.DATE", "REFERRAL.COUNTY.CODE", "AGE.AT.REFERRAL", "REPORT_YEAR")] <-
  "character"

jcpss <- read.csv(rawFile, colClasses = colClasses)
jcpss <- jcpss[,-1L] # remove row names

rm(rawFile, colNames, colClasses)

for (colName in c("REFERRAL.COUNTY.CODE", "AGE.AT.REFERRAL", "REPORT_YEAR"))
  jcpss[[colName]] <- as.integer(jcpss[[colName]])
rm(colName)

colnames(jcpss) <- tolower(colnames(jcpss))
colnames(jcpss) <- gsub("\\.", "_", colnames(jcpss))

## prunes any blanks
for (i in seq_along(jcpss)) {
  if (is.factor(jcpss[[i]])) {
    jcpss[[i]][jcpss[[i]] == ""] <- NA
    jcpss[[i]] <- droplevels(jcpss[[i]])
  }
}
rm(i)

action_date <- as.Date(jcpss$action_date, "%Y-%m-%d")
action_date[is.na(action_date)] <- as.Date(jcpss$action_date[is.na(action_date)], "%m/%d/%Y 0:00")
jcpss$action_date <- action_date
rm(action_date)

## remap mispellings
commonSrcPath <- file.path("..", "common", "src")
source(file.path(commonSrcPath, "util.R"))

jcpss$action_type <-
  remapFactor(jcpss$action_type,
  list("1", "2", "Petiton"),
  c("Referral", "Court", "Petition"))

jcpss$referral_source <- remapFactor(jcpss$referral_source,
   c("Lae Enforcement Agency", "Law Enforcement", "Law Enforcement Agency"),
  "Law Enforcement Agency")

jcpss$detention <- remapFactor(jcpss$detention,
  c("Detained - Secure Facility", "Detained-Secure Facility"),
  "Detained - Secure Facility")

jcpss$offense_level <- remapFactor(jcpss$offense_level,
  c("Misdemeanor", "MIsdemeanor"),
  "Misdemeanor")

informalProbationCategories <- c("Informal Probation (654.2 WI)", "Informal Probation (654.2WI)", "Informal Probation(654.2WI)")
nonwardProbationCategories  <- c("Non-Ward Probation (725a WI)", "Non_Ward Probation (725a WI)", "Non-Ward Probation (725(a) WI)",
                                 "Non-Ward Probation (725a WI0", "Non-Ward Probation")
deferredEntryCategories     <- c("Deferred Entry of Judgement", "Deferred Entry of Judgment")
remandedCategorires         <- c("Remanded to Adult Court", "Remand to Adult Court")
electronicWardshipCategories <- c("Wardship (Secure County Facility - with Elect. Monitoring)",
                                  "Wardship (Secure County Facility - Includes Electronic Monitoring)")
jcpss$disposition <- remapFactor(jcpss$disposition,
  list(informalProbationCategories, nonwardProbationCategories, deferredEntryCategories, remandedCategorires, electronicWardshipCategories),
  c("Informal Probation (654.2 WI)", "Non-Ward Probation (725a WI)", "Deferred Entry of Judgement", "Remanded to Adult Court",
    "Wardship (Secure County Facility - with Elect. Monitoring)"))
rm(informalProbationCategories, nonwardProbationCategories, deferredEntryCategories, remandedCategorires, electronicWardshipCategories)

save(jcpss, file = file.path("datasets", "jcpss", "jcpss_clean.Rdata"))
