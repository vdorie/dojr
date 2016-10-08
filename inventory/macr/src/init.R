bcsOffenseCodes <- read.csv(file.path(dataPath, "bcs_offense_codes.csv"), stringsAsFactors = FALSE)

source(file.path(commonSrcPath, "bcsOffenseCodes.R"))

jurisdictions <- read.csv(file.path(dataPath, "jurisdictions.csv"), stringsAsFactors = FALSE)

source(file.path(commonSrcPath, "jurisdictions.R"))
