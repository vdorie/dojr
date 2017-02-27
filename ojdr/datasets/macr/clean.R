connectToDatabase <- function(drv, dbname)
{
  credentials <- read.table(file.path("dbCredentials.properties"), sep = "=", strip.white = TRUE, stringsAsFactor = FALSE)
  for (i in seq_len(nrow(credentials))) assign(credentials[i,1L], credentials[i,2L])
  
  dbConnect(drv, dbname = dbname,
            host = host, port = port,
            user = user, password = password)
}

if (require(RPostgreSQL, quietly = TRUE) == FALSE) {
  repos <- getOption("repos")
  if (is.null(repos)) repos <- getCRANmirrors()$URL[1L]
  install.packages("PostgreSQL", repos, dependencies = TRUE)
  if (require(RPostgreSQL, quietly = TRUE) == FALSE) stop("could not install RPostgreSQL package, do so manually")
}

drv <- dbDriver("PostgreSQL")

con <- connectToDatabase(drv, "macr")

tableDef <- read.csv(file.path("datasets", "macr", "tableDef.csv"))
isEnumType <- function(x) grepl(";", x)

if (FALSE) {
macr <- dbGetQuery(con, paste0("SELECT ", tableDef$full_name[1L], " FROM macr_typed"))

for (i in seq.int(2L, nrow(tableDef))) {
  values <- dbGetQuery(con, paste0("SELECT ", tableDef$full_name[2L], " FROM macr_typed"))
  if (isEnumType(tableDef$type[i])) values <- as.factor(values)
  
  macr[[tableDef$full_name[i]]] <- values
  
  if (i %% 5L == 0L) gc(invisible(FALSE))
}
}

macr <- dbGetQuery(con, paste0("SELECT * FROM macr_typed"))


dbDisconnect(con)

dbUnloadDriver(drv)

macr[["blank"]] <- NULL
for (variableName in c("reference_number", "name", "birth_year", "birth_month", "birth_day"))
  macr[[variableName]] <- factor(rep_len(1L, nrow(macr)), labels = "pii")
invisible(gc(FALSE))

for (variableName in c("record_id", "bcs_jurisdiction", "ncic_jurisdiction", "summary_offense_level", "offense_level",
                       "bcs_offense_code", "bcs_summary_offense_code", "fbi_offense_code", "race", "gender", "status_type",
                       "disposition"))
  macr[[variableName]] <- as.factor(macr[[variableName]])
invisible(gc(FALSE))

source(file.path("..", "common", "src", "util.R"))

macr <- subset(macr, !is.na(age) | !is.na(race) | !is.na(gender))

for (i in seq_along(macr))
  if (is.factor(macr[[i]])) macr[[i]] <- droplevels(macr[[i]])

invisible(gc(FALSE))

macr$bcs_jurisdiction[macr$bcs_jurisdiction == "     "] <- NA
macr$bcs_jurisdiction <- droplevels(macr$bcs_jurisdiction)

macr$ncic_jurisdiction[macr$ncic_jurisdiction == "  \177\177"] <- NA
macr$ncic_jurisdiction <- droplevels(macr$ncic_jurisdiction)

macr$fbi_offense_code[macr$fbi_offense_code == "    "] <- NA
macr$fbi_offense_code <- droplevels(macr$fbi_offense_code)
levels(macr$fbi_offense_code) <- trimws(levels(macr$fbi_offense_code))


for (variableName in c("offense_level", "disposition"))
  levels(macr[[variableName]]) <- gsub("_", " ", levels(macr[[variableName]]))

macr$race <- remapFactor(macr$race,
                         list(c("American_Indian", "American_Indian_i"), c("black", "black_b"), c("Chinese", "Chinese_c"),
                              c("Filipino", "Filipino_f"), c("Hispanic", "Hispanic_h"), c("Japanese", "Japanese_j"),
                              c("other", "other_o"),  "other_Asian", c("Pacific_Islander", "Pacific_Islander_p"),
                              c("white", "white_w"), "Asian_Indian"),
                         c("American Indian", "black", "Chinese", "Filipino", "Hispanic", "Japanese", "other", "other Asian",
                           "Pacific Islander", "white", "Asian Indian"))

save(macr, file = file.path("datasets", "macr", "macr_clean.Rdata"))
