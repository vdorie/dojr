rawFile <- file.path("datasets", "jcpss", "raw", "jcpss_OJ.csv")

#colNames <- readLines(rawFile, n = 1L)
#colNames <- strsplit(colNames, ",")[[1L]]
#colNames <- sub("\"(.*)\"", "\\1", colNames)

jcpss <- read.csv(rawFile, colClasses = "factor")

rm(rawFile)

jcpss <- jcpss[-150477,] # remove aberrant row

## remove excess whitespace from columns
for (i in seq_along(jcpss)) {
  jcpss[[i]] <- droplevels(jcpss[[i]])
  levels(jcpss[[i]]) <- trimws(levels(jcpss[[i]]))
}

for (colName in c("REPORT_YEAR", "REFERRAL_COUNTY_CODE", "AGE_AT_REFERRAL"))
  jcpss[[colName]] <- as.integer(levels(jcpss[[colName]]))[jcpss[[colName]]]
rm(colName)

jcpss$ACTION_DATE <- as.Date(levels(jcpss$ACTION_DATE), "%m/%d/%Y")[jcpss$ACTION_DATE]


## prunes any blanks
for (i in seq_along(jcpss)) {
  if (is.factor(jcpss[[i]])) {
    jcpss[[i]][jcpss[[i]] == ""] <- NA
    jcpss[[i]] <- droplevels(jcpss[[i]])
  }
}
rm(i)

dispositionMap <- as.data.frame(matrix(c(
    "10", "Dismissed",
    "12", "Transferred",
    "13", "Remanded to Adult Court",
    "14", "Deported",
    "15", "Traffic Court",
    "16", "Direct File - Adult Court",
    "20", "Informal Probation (654 WI)",
    "21", "Informal Probation (654.2 WI)",
    "30", "Non-Ward Probation (725(a) WI)",
    "40", "Wardship (Own/Relative's Home)",
    "41", "Wardship (Non-secure County Facility)",
    "42", "Wardship (Secure County Facility - Includes Electronic Monitoring)",
    "43", "Wardship (Other Public Facility)",
    "44", "Wardship (Private Facility)",
    "49", "Wardship (Other)",
    "50", "Wardship (CYA)",
    "60", "Diversion",
    "61", "Deferred Entry of Judgment"),
  byrow = TRUE, ncol = 2L,
  dimnames = list(NULL, c("code", "name"))), stringsAsFactors = FALSE)

levels(jcpss$DISPOSITION) <- dispositionMap$name[match(levels(jcpss$DISPOSITION), dispositionMap$code)]

colnames(jcpss) <- tolower(colnames(jcpss))
colnames(jcpss) <- gsub("\\.", "_", colnames(jcpss))

## remap mispellings
commonSrcPath <- file.path("..", "common", "src")
source(file.path(commonSrcPath, "util.R"))

jcpss$referral_source <- remapFactor(jcpss$referral_source,
   c("Lae Enforcement Agency", "Law Enforcement Agency"),
  "Law Enforcement Agency")

jcpss$detention <- remapFactor(jcpss$detention,
  c("Detained - Secure Facility", "Detained-Secure Facility"),
  "Detained - Secure Facility")

## rename a few columns
colnames(jcpss)[colnames(jcpss) %in% "fitness_hearing_out_desc"] <- "fitness_hearing_result"
colnames(jcpss)[colnames(jcpss) %in% "county_desc"] <- "referral_county_name"

save(jcpss, file = file.path("datasets", "jcpss", "jcpss_clean.Rdata"))
