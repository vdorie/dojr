macr <- read.csv("macr.csv")
counties <- read.csv("counties.csv")

## replace county codes with names
macr$county <- counties$name[match(macr$county, counties$code)]
rm(counties)

macr$disposition <- NULL

## we table using this combined key and then split it out into cells later
macr$key <- as.factor(with(macr, paste0(year, ":", county, ":", gender, ":", race, ":", age)))

tab <- table(macr[,c("key", "offense_code")], useNA = "ifany")
rm(macr)

macr.sum <- as.data.frame.matrix(tab)
macr.sum[c("year", "county", "gender", "race", "age")] <- t(sapply(strsplit(rownames(tab), ":"), function(x) x))
rm(tab)

macr.sum$gender <- as.factor(macr.sum$gender)
macr.sum$race <- as.factor(macr.sum$race)
macr.sum$age <- as.factor(macr.sum$age)


rownames(macr.sum) <- NULL
macr.sum <- macr.sum[c(seq.int(ncol(macr.sum) - 4L, ncol(macr.sum)), seq_len(ncol(macr.sum) - 5L))]

macr.sum$HOMICIDE   <- rowSums(macr.sum[,c("1", "2")])
macr.sum$VIOLENT    <- rowSums(macr.sum[,c("1", "2", "4", "5", "6", "7")])
macr.sum$PROPERTY   <- rowSums(macr.sum[,c("8", "9", "10", "11", "24")])
macr.sum$F_DRUGOFF  <- rowSums(macr.sum[,c("12", "13", "14", "15")])
macr.sum$F_SEXOFF   <- rowSums(macr.sum[,c("16", "17", "18")])
macr.sum$F_ALLOTHER <- rowSums(macr.sum[,c("3", "19", "20", "21", "22", "23", "25")])
macr.sum$F_TOTAL    <- rowSums(macr.sum[,c("VIOLENT", "PROPERTY", "F_DRUGOFF", "F_SEXOFF", "F_ALLOTHER")])
macr.sum$M_TOTAL    <- rowSums(macr.sum[,c("29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "76")])
macr.sum$S_TOTAL    <- 0 # rowSums(macr.sum[,c("68", "69", "70", "71", "72")])

offenseCodeColumns <- grepl("[0-9]+", colnames(macr.sum))
colnames(macr.sum)[offenseCodeColumns] <- sprintf("SCO%02d_sum", as.integer(colnames(macr.sum)[offenseCodeColumns]))

colnames(macr.sum)[colnames(macr.sum) == "year"] <- "YEAR"
colnames(macr.sum)[colnames(macr.sum) == "gender"] <- "GENDER"
colnames(macr.sum)[colnames(macr.sum) == "race"] <- "RACE"
colnames(macr.sum)[colnames(macr.sum) == "age"] <- "AGE_GROUP"
colnames(macr.sum)[colnames(macr.sum) == "county"] <- "COUNTY"

levels(macr.sum$AGE_GROUP) <- c("18 to 24", "25 to 32", "33 to 44", "45 and over")

## state aggregates
macr.sum$key <- as.factor(with(macr.sum, paste0(YEAR, ":", GENDER, ":", RACE, ":", AGE_GROUP)))
totals <- t(sapply(levels(macr.sum$key), function(key.i) {
  colSums(subset(macr.sum, key == key.i,
                 setdiff(colnames(macr.sum), c("YEAR", "COUNTY", "GENDER", "RACE", "AGE_GROUP", "key"))))
  }))
macr.sum$key <- NULL

totals <- as.data.frame(totals)
key <- strsplit(rownames(totals), ":")
totals$YEAR      <- as.integer(sapply(key, function(key.i) key.i[1L]))
totals$COUNTY    <- "Statewide"
totals$GENDER    <- as.factor(sapply(key, function(key.i) key.i[2L]))
totals$RACE      <- as.factor(sapply(key, function(key.i) key.i[3L]))
totals$AGE_GROUP <- as.factor(sapply(key, function(key.i) key.i[4L]))

firstCols <- c("YEAR", "COUNTY", "GENDER", "RACE", "AGE_GROUP")
totals <- totals[, c(firstCols, setdiff(colnames(totals), firstCols))]
rownames(totals) <- NULL

macr.sum <- rbind(macr.sum, totals)

write.csv(macr.sum, file = "macr_summary.csv", row.names = FALSE)
