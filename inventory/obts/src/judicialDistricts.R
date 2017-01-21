## cleans the csv file into something more structured
## run from dojr/inventory/obts by source("src/judicialDistricts.R")
## assumes judicial_districts.csv exists in dojr/inventory/obts

judicialDistricts <- read.csv("judicial_districts.csv")
judicialDistricts$county_code <- as.factor(substr(as.character(judicialDistricts$code), 1L, 2L))
judicialDistricts$agency_code <- substr(as.character(judicialDistricts$code), 3L, 6L)
judicialDistricts$code <- NULL
judicialDistricts <- judicialDistricts[c(2L, 3L, 1L)]

countyNames <- strsplit(as.character(judicialDistricts$name), " - ")
nonstandardNames <- which(sapply(countyNames, length) != 2L)

#> judicialDistricts[nonstandardNames,]
#    county_code agency_code                                            name
#236          29        660J                      Nevada County Court Nevada
#257          31        620J Placer County Court - Colfax- Alta - Dutch Flat
#265          32        630J  Plumas County Court - Almanor - Chester Branch
#289          34        630J           Sacramento Court - Fair Oaks - Folsom
#293          35        640J                   San Benito Municip- Hollister
#378          45        633J              Shasta County Court Central Valley
#457          55        650J              Tuolumne Court _ Fifth (Jamestown)
countyNames[[nonstandardNames[1L]]] <- c("Nevada County Court", "Nevada")
countyNames[[nonstandardNames[2L]]] <- c("Placer County Court", "Colfax/Alta/Dutch Flat")
countyNames[[nonstandardNames[3L]]] <- c("Plumas County Court", "Almanor/Chester Branch")
countyNames[[nonstandardNames[4L]]] <- c("Sacramento County Court", "Fair Oaks/Folsom")
countyNames[[nonstandardNames[5L]]] <- c("Shasta County Court", "Central Valley")
countyNames[[nonstandardNames[6L]]] <- c("San Benito County Court", "Hollister")
countyNames[[nonstandardNames[7L]]] <- c("Tuolumne Court", "Fifth (Jamestown)")
rm(nonstandardNames)

countyNames <- unlist(countyNames)
countyNames <- data.frame(county = countyNames[seq(1L, length(countyNames) - 1L, by = 2L)],
                          agency = countyNames[seq(2L, length(countyNames), by = 2L)],
                          stringsAsFactors = FALSE)

namesWithoutCounty <- !grepl("County", countyNames$county, ignore.case = TRUE)
countyNames$county[namesWithoutCounty] <- sub("Court", "County Court", countyNames$county[namesWithoutCounty])
rm(namesWithoutCounty)

splitNames <- unlist(strsplit(countyNames$county, " County "))
countyNames <- data.frame(county = splitNames[seq(1L, length(splitNames) - 1L, by = 2L)],
                          type   = splitNames[seq(2L, length(splitNames), by = 2L)],
                          agency = countyNames$agency)
levels(countyNames$type) <- c("Arrest", "County", "Juvenile", "Superior")

judicialDistricts$name <- NULL
judicialDistricts$county <- countyNames$county
judicialDistricts$type   <- countyNames$type
judicialDistricts$agency <- as.character(countyNames$agency)

judicialDistricts$county[judicialDistricts$county == "El Dorada"] <- "El Dorado"
judicialDistricts$county <- droplevels(judicialDistricts$county)

judicialDistricts$agency_code[judicialDistricts$agency_code == "****"] <- NA

write.csv(judicialDistricts, file.path("..", "..", "common", "data", "judicial_districts.csv"),
          row.names = FALSE,
          quote = 1L)
