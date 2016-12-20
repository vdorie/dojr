# in order to connect to a database, this requires a properties file in the src folder named
# 'dbCredentials.properties' with format:
#
#   host=host
#   port=port
#   user=unprivileged_account_name
#   password=unprivileged_account_password
#   user.su=privileged_account_name
#   password.su=privileged_account_password
#
# only the superuser needs to be defined, as unprivileged user will be created
# finally, the superuser can't actually be avoided, since it is needed to COPY files
# into tables


## what we connect to if all else fails
adminDatabase <- "postgres"

createDatabase <- function(drv, dbname, properties = "")
{
  credentials <- read.table(file.path("src", "dbCredentials.properties"), sep = "=", strip.white = TRUE, stringsAsFactor = FALSE)
  for (i in seq_len(nrow(credentials))) assign(credentials[i,1L], credentials[i,2L])
  
  con <- dbConnect(drv, dbname = adminDatabase,
                   host = host, port = port,
                   user = user.su, password = password.su)
  
  connectionResult <- tryCatch(dbExecute(con, paste0("CREATE DATABASE ", dbname, if (properties != "") paste0(" WITH ", properties) else "")), error = function(e) e)
  dbDisconnect(con)
  
  connectionResult
}

connectToDatabase <- function(drv, dbname, superuser = FALSE)
{
  credentials <- read.table(file.path("src", "dbCredentials.properties"), sep = "=", strip.white = TRUE, stringsAsFactor = FALSE)
  for (i in seq_len(nrow(credentials))) assign(credentials[i,1L], credentials[i,2L])
  
  ## used if we need to create a database
  dbProperties <- "OWNER = postgres ENCODING = 'UTF8' LC_COLLATE = 'C' LC_CTYPE = 'C' TABLESPACE = pg_default CONNECTION LIMIT = -1"
  
  if (superuser) {
    ## if this fails, that likely means that the database does not exist
    connectionResult <-
      tryCatch(con <-
        dbConnect(drv, dbname = dbname,
                  host = host, port = port,
                  user = user.su, password = password.su),
                  error = function(e) e)
    if (is(connectionResult, "error")) {
      createDatabase(drv, dbname, dbProperties)
      connectionResult <-
        tryCatch(con <-
          dbConnect(drv, dbname = dbname,
                    host = host, port = port,
                    user = user.su, password = password.su),
                    error = function(e) e)
    }
  } else {
    ## this can fail when the db doesn't exist or the user doesn't exist
    connectionResult <-
      tryCatch(con <-
        dbConnect(drv, dbname = dbname,
                  host = host, port = port,
                  user = user, password = password),
                  error = function(e) e)
    if (is(connectionResult, "error")) {
      ## let this throw an error if it fails
      con.su <- dbConnect(drv, dbname = adminDatabase, host = host, port = port, user = user.su, password = password.su)
      roleString <- paste0(user, " WITH
        LOGIN
        PASSWORD '", password, "' 
        NOSUPERUSER
        INHERIT
        NOCREATEDB
        NOCREATEROLE
        NOREPLICATION")
      if (nrow(dbGetQuery(con.su, paste0("SELECT * FROM pg_roles WHERE rolname = '", user, "'"))) == 0L) {
        dbExecute(con, paste0("CREATE USER ", roleString))
      } else {
        dbExecute(con, paste0("ALTER USER ", roleString))
      }
      
      createDatabase(drv, dbname, dbProperties)
      
      dbDisconnect(con.su)
      
      connectionResult <-
        tryCatch(con <-
          dbConnect(drv, dbname = dbname,
                    host = host, port = port,
                    user = user, password = password),
                    error = function(e) e)
    }
  }
  
  connectionResult
}

dbExistsType <- function(con, name)
  dbGetQuery(con, paste0("SELECT EXISTS(SELECT 1 FROM pg_type WHERE typname = '", name, "')"))[[1L]]

## used to reset some stuff for testing purposes
dropTable <- function(con, tableDef, tableNames)
{
  ignored <- function(e) invisible(NULL)
  
  if ("obts_raw_1" %in% tableNames) {
    if (dbExistsTable(con, "obts_raw_1")) dbExecute(con, "DROP TABLE obts_raw_1")
    if (dbExistsTable(con, "info")) dbExecute(con, "UPDATE info SET raw_id_start = 0, raw_id_end = 0 WHERE format = 1")
  }
  if ("obts_raw_2" %in% tableNames) {
    if (dbExistsTable(con, "obts_raw_2")) dbExecute(con, "DROP TABLE obts_raw_2")
    if (dbExistsTable(con, "info")) dbExecute(con, "UPDATE info SET raw_id_start = 0, raw_id_end = 0 WHERE format = 2")
  }
  if ("obts" %in% tableNames) {
    if (dbExistsTable(con, "obts")) dbExecute(con, "DROP TABLE obts")
    
    if (dbExistsTable(con, "info")) dbExecute(con, "UPDATE info SET id_start = 0, id_end = 0")
  }
  if ("obts_typed" %in% tableNames) {
    if (dbExistsTable(con, "obts")) dbExecute(con, "DROP TABLE obts_typed")
    
    for (i in seq_len(nrow(tableDef))) {
      if (!grepl(";", tableDef$type[i])) next
      typeName <- paste0("obts_", tableDef$full_name[i])
      if (dbExistsType(con, typeName)) dbExecute(con, paste0("DROP TYPE ", typeName))
    }
  }
    
  if ("info" %in% tableNames && dbExistsTable(con, "info")) dbExecute(con, "DROP TABLE info")
  
  invisible(NULL)
}

createTablesIfNonexistent <- function(drv, tableDef, tableNames)
{
  con <- connectToDatabase(drv, "obts")
  
  if ("info" %in% tableNames && !dbExistsTable(con, "info")) {
    dbExecute(con,
      "CREATE TABLE info (
         file_name      varchar(64),
         size           bigint,
         timestamp      timestamp,
         hash           varchar(32),
         format         smallint,
         id_start       bigint,
         id_end         bigint,
         raw_id_start   bigint,
         raw_id_end     bigint
       )")
  }
  if ("obts_raw_1" %in% tableNames && !dbExistsTable(con, "obts_raw_1")) {
    dbExecute(con,
      "CREATE TABLE obts_raw_1 (
         id         bigserial,
         record     character(176)
       )")
  }
  if ("obts_raw_2" %in% tableNames && !dbExistsTable(con, "obts_raw_2")) {
    dbExecute(con,
      "CREATE TABLE obts_raw_2 (
         id         bigserial,
         record     character(184)
       )")
  }
  if ("obts" %in% tableNames && !dbExistsTable(con, "obts")) {
    createString <- "CREATE TABLE obts (\n"
    nameLength <- 2L + max(nchar(tableDef$abbreviation))
    
    for (i in seq_len(nrow(tableDef))) {
      if (grepl("serial", tableDef$type[i])) 
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$abbreviation[i]), tableDef$type[i], " PRIMARY KEY")
      else
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$abbreviation[i]), "character(", tableDef$length_2[i], ")")
      
      createString <- paste0(createString, if (i != nrow(tableDef)) ",\n" else "\n")
    }
    createString <- paste0(createString, ")")
    
    dbExecute(con, createString)
  }
    
  if ("obts_typed" %in% tableNames && !dbExistsTable(con, "obts_typed")) {
    ## can probably move these typedefs into a schema to help with namespace stuff
    
    for (i in seq_len(nrow(tableDef))) {
      if (grepl(";", tableDef$type[i])) {
        enumSpec <- strsplit(strsplit(tableDef$type[i], "; ")[[1L]], " = ")
        temp <- sapply(enumSpec, function(x) x[2L])
        names(temp) <- sapply(enumSpec, function(x) sub("'(.*)'", "\\1", x[1L]))
        enumSpec <- temp; rm(temp)
        
        name <- paste0("obts_", tableDef$full_name[i])
        if (dbExistsType(con, name)) {
          warning("type '", name, "' already exists, dropping")
          dbExecute(con, paste0("DROP TYPE ", name))
        }
        createString <- paste0("CREATE TYPE ", name, " AS ENUM ('", paste0(enumSpec, collapse = "', '"), "')")
        dbExecute(con, createString)
      }
    }
    
    ## this is hacky, as it references the obts key to create a foreign constraint
    createString <- "CREATE TABLE obts_typed (\n"
    nameLength <- 2L + max(nchar(tableDef$full_name))
    for (i in seq_len(nrow(tableDef))) {
      if (grepl("serial", tableDef$type[i])) {
        keyType <- sub("serial", "int", tableDef$type[i])
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$full_name[i]), keyType, " REFERENCES obts(db_id)")
      } else if (!grepl(";", tableDef$type[i])) {
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$full_name[i]), tableDef$type[i])
      } else {
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$full_name[i]), "obts_", tableDef$full_name[i])
      }
      createString <- paste0(createString, if (i != nrow(tableDef)) ",\n" else "\n")
    }
    createString <- paste0(createString, ")")
    
    dbExecute(con, createString)
  }
  
  dbDisconnect(con)
  
  invisible(NULL)
}


defineImportFunctions <- function(con, tableDef) {
  dbExecute(con,
    "CREATE OR REPLACE FUNCTION string_to_int(text) RETURNS INT AS $$
       SELECT CASE
         WHEN trim($1) SIMILAR TO '[0-9]+' THEN CAST(trim($1) AS INT)
         ELSE NULL
       END;
     $$ LANGUAGE SQL")
  
  dbExecute(con,
    "CREATE OR REPLACE FUNCTION is_valid_date(text) RETURNS BOOLEAN LANGUAGE plpgsql IMMUTABLE AS $$
     BEGIN
       RETURN CASE WHEN $1::date IS NULL THEN false ELSE true END;
     exception WHEN others THEN
       RETURN false;
     END;$$;")
  
  dbExecute(con,
    "CREATE OR REPLACE FUNCTION string_to_date(text, text) RETURNS date AS $$
       SELECT CASE
         WHEN is_valid_date($1) THEN to_date($1, $2) ELSE NULL END;
     $$ LANGUAGE SQL")
  
  for (i in seq_len(nrow(tableDef))) {
    if (!grepl(";", tableDef$type[i])) next
    
    enumSpec <- strsplit(strsplit(tableDef$type[i], "; ")[[1L]], " = ")
    temp <- sapply(enumSpec, function(x) x[2L])
    names(temp) <- sapply(enumSpec, function(x) sub("'(.*)'", "\\1", x[1L]))
    enumSpec <- temp; rm(temp)

    enumName <- tableDef$full_name[i]
    
    createString <- paste0("CREATE OR REPLACE FUNCTION string_to_", enumName, "(text) RETURNS obts_", enumName, " AS $$\n",
      "  SELECT CASE\n")
    hasElse <- FALSE
    for (j in seq_along(enumSpec)) {
      if (names(enumSpec)[j] != "") {
        createString <- paste0(createString, "    WHEN $1 = '", names(enumSpec)[j], "' THEN CAST('", enumSpec[j], "' AS obts_", enumName, ")\n")
      } else {
        createString <- paste0(createString, "    ELSE CAST('", enumSpec[j], "' AS obts_", enumName, ")\n")
        hasElse <- TRUE
      }
    }
    if (!hasElse) createString <- paste0(createString, "    ELSE NULL\n")
    createString <- paste0(createString, "  END;\n$$ LANGUAGE SQL")
    createResult <- tryCatch(dbExecute(con, createString), error = function(e) e)
    if (is(createResult, "error")) browser()
  }
   
  invisible(NULL)
}

dbExistsFunction <- function(con, name, args)
{
  dbExecute(con, "CREATE OR REPLACE FUNCTION public.function_arguments(oid)
                  RETURNS text LANGUAGE sql AS $function$
                    select string_agg(par, ', ') 
                      from (select format_type(unnest(proargtypes), null) par 
                        from pg_proc where oid = $1) x
                  $function$")
  dbGetQuery(con, paste0("SELECT EXISTS(SELECT * FROM pg_proc WHERE proname = '", name, "' AND function_arguments(oid) = '", args, "')"))[[1L]][[1L]]
}

dropImportFunctions <- function(con, tableDef) {
  if (dbExistsFunction(con, "string_to_int", "text")) dbExecute(con, "DROP FUNCTION string_to_int(text)")
  if (dbExistsFunction(con, "string_to_date", "text, text")) dbExecute(con, "DROP FUNCTION string_to_date(text, text)")
  if (dbExistsFunction(con, "is_valid_date", "text")) dbExecute(con, "DROP FUNCTION is_valid_date(text)")
  
  for (i in seq_len(nrow(tableDef))) {
    if (!grepl(";", tableDef$type[i])) next
    
    functionName <- paste0("string_to_", tableDef$full_name[i])
    if (dbExistsFunction(con, functionName, "text"))
      dbExecute(con, paste0("DROP FUNCTION ", functionName, "(text)"))
  }
  invisible(NULL)
}


insertFileIntoInfoTable <- function(con, inputPath, fileName) {
  fileInfo <- file.info(file.path(inputPath, fileName))
  hash     <- tools::md5sum(file.path(inputPath, fileName))
  
  insertStatement <- paste0(
    "INSERT INTO info VALUES
      ('", paste(fileName, fileInfo$size, format(fileInfo$mtime, format = "%F %X %z"), hash,
                 0, 0, 0, 0, 0, sep = "', '"), "')")
  dbExecute(con, insertStatement)
  
  invisible(NULL)
}

deleteDataRowsForEntry <- function(con, entry) {
  if (entry$id_start != 0L) {
    dbExecute(con, paste0("DELETE FROM obts_typed WHERE id BETWEEN ", entry$id_start, " AND ", entry$id_end))
    dbExecute(con, paste0("DELETE FROM obts WHERE db_id BETWEEN ", entry$id_start, " AND ", entry$id_end))
  }
  
  if (entry$raw_id_start != 0L) {
    rawTableName <- paste0("obts_raw_", entry$format)
    dbExecute(con, paste0("DELETE FROM ", rawTableName, " WHERE id BETWEEN ", entry$raw_id_start, " AND ", entry$raw_id_end))
  }
  invisible(NULL)
}

updateInInfoTableAndDeleteDataRows <- function(con, inputPath, fileName, currentEntry) {
  deleteDataRowsForEntry(con, currentEntry)
  
  fileInfo <- file.info(file.path(inputPath, fileName))
  hash     <- tools:md5sum(file.path(inputPath, fileName))
  
  updateStatement <- paste0("UPDATE info SET ",
    "timestamp = '", format(fileInfo$mtime, format = "%F %X %z"), "', ",
    "hash = '", hash, "', ",
    "size = '", fileInfo$size, "', ",
    "format = 0, id_start = 0, id_end = 0, raw_id_start = 0, raw_id_end = 0 ",
    "WHERE file_name = '", fileName, "'")
  dbExecute(con, updateStatement)
}

importIntoRawTable <- function(con, inputPath, fileName)
{
  inputFile <- file.path(inputPath, fileName)
  ## cannot use system tempdir due to permissions
  tempFile <- tempfile(tmpdir = normalizePath("."))
  
  ## some files have null bytes as padding and they can't be put in character
  ## fields; use tr to replace with spaces
  system2("tr", args = "'\\000' ' '",
          stdin = inputFile,
          stdout = tempFile)
  lineLength <- nchar(readLines(tempFile, n = 1L))
  
  inputFormat <- switch(as.character(lineLength), "176" = 1L, "184" = 2L, NULL)
  if (is.null(inputFormat)) {
    unlink(tempFile)
    stop("unrecognized format for file: ", fileName)
  }
  rawTableName <- paste0("obts_raw_", inputFormat)
  
  ## There is definitely a more elegant way of doing this, but we first get
  ## the sequence var which increments it, set it back one, copy, and count
  ## how many rows were added. The table should probably be locked for this.
  rawStartIndex <- dbGetQuery(con, paste0("SELECT nextval('", rawTableName, "_id_seq')"))[[1L]]
  invisible(dbGetQuery(con, paste0("SELECT setval('", rawTableName, "_id_seq', ", rawStartIndex, ", false)")))
 
  importStatement <- paste0(
    "COPY ", rawTableName, " (record)
       FROM '", tempFile, "'
       WITH (FORMAT text)")
  
  rawEndIndex <- rawStartIndex + dbExecute(con, importStatement)[[1L]] - 1L
  unlink(tempFile)

  updateStatement <- paste0(
    "UPDATE info SET
       format = '", inputFormat, "',
       raw_id_start = ", rawStartIndex, ",
       raw_id_end = ", rawEndIndex, "
       WHERE 
       file_name = '", fileName, "'")
  dbExecute(con, updateStatement)
}

updateInfoTable <- function(drv, tableDef, inputPath) {
  createTablesIfNonexistent(drv, tableDef, "info")
  
  inputFiles <- list.files(inputPath)
  
  con <- connectToDatabase(drv, "obts")
  currentEntries <- dbGetQuery(con, "SELECT * FROM info")
  
  for (fileName in inputFiles) {
    if (!(fileName %in% currentEntries$file_name)) {
      cat("inserting file '", fileName, "' into info table\n", sep = "")
      
      insertFileIntoInfoTable(con, inputPath, fileName)
      next
    }
    
    currentEntry <- currentEntries[currentEntries$file_name == fileName,]
    
    fileInfo <- file.info(file.path("input", fileName))
  
    if (fileInfo$mtime == currentEntry$timestamp && fileInfo$size == currentEntry$size) next
  
    ## only timestamps differ, check the hash
    if (fileInfo$size == currentEntry$size) {
      hash <- tools::md5sum(file.path("input", inputFile))
      if (hash == currentEntry$hash) {
        cat("updating timestamp for file '", fileName, "' in info table\n", sep = "")
        
        dbExecute(con, paste0("UPDATE info SET timestamp = '", format(fileInfo$mtime, format = "%F %X %z"),
                            "' WHERE file_name = '", inputFile, "'"))
        next
      }
    }
    
    cat("updating file '", inputFile, "' in info table\n", sep = "")
    updateInInfoTableAndDeleteDataRows(con, inputFile, rs)
  }
  
  if (any(!(currentEntries$file_name %in% inputFiles))) {
    extraFiles <- currentEntries[!(currentEntries$file_name %in% inputFiles),]
    
    for (i in seq_len(nrow(extraFiles))) {
      extraFileEnry <- extraFiles[i,]
      cat("deleting extra file '", extraFileEntry$file_name, "'\n", sep = "")
      deleteDataRowsForEntry(con, extraFileEntry)
      dbExecute(con, paste0("DELETE FROM info WHERE file_name = '", extraFileEntry$file_name, "'"))
    }
  }
  
  dbDisconnect(con)
  
  invisible(NULL)
}

updateRawTables <- function(drv, tableDef, inputPath) {
  createTablesIfNonexistent(drv, tableDef, c("obts_raw_1", "obts_raw_2"))
  
  con <- connectToDatabase(drv, "obts")
  currentEntries <- dbGetQuery(con, "SELECT * FROM info WHERE raw_id_start = 0")
  
  if (nrow(currentEntries) > 0L) {
    con.su <- connectToDatabase(drv, "obts", TRUE)
    for (i in seq_len(nrow(currentEntries))) {
      currentEntry <- currentEntries[i,]
      
      cat("importing raw file '", currentEntry$file_name, "'\n", sep = "")
      importIntoRawTable(con.su, inputPath, currentEntry$file_name)
    }
    dbDisconnect(con.su)
  }
  
  dbDisconnect(con)
  
  invisible(NULL)
}

parseRawColumnsForEntry <- function(con, tableDef, entry) {
  insertStatement <- "INSERT INTO obts_typed ("
  for (i in seq_len(nrow(tableDef))) {
    insertStatement <- paste0(insertStatement, tableDef$full_name[i])
    if (i != nrow(tableDef)) insertStatement <- paste0(insertStatement, ", ")
  }
  insertStatement <- paste0(insertStatement, ")\n")
  
  indentLength <- 9L
  insertStatement <- paste0(insertStatement, "  SELECT\n")
  for (i in seq_len(nrow(tableDef))) {
    insertStatement <- paste0(insertStatement, sprintf("%-*s", indentLength, ""))
    
    if (grepl("int$", tableDef$type[i])) {
      insertStatement <- paste0(insertStatement, "string_to_int(obts.", tableDef$abbreviation[i], ") AS ", tableDef$full_name[i])
    } else if (tableDef$type[i] == "date") {
      insertStatement <- paste0(insertStatement, "string_to_date(obts.", tableDef$abbreviation[i], ", '",
                                if (entry$format == 1L) "YYMMDD" else "YYYYMMDD", "') AS ", tableDef$full_name[i])
    } else if (grepl(";", tableDef$type[i])) {
      insertStatement <- paste0(insertStatement, "string_to_", tableDef$full_name[i], "(obts.", tableDef$abbreviation[i], ") AS ", tableDef$full_name[i])
    } else {
      insertStatement <- paste0(insertStatement, "obts.", tableDef$abbreviation[i], " AS ", tableDef$full_name[i])
    }
   
    insertStatement <- paste0(insertStatement, if (i != nrow(tableDef)) ",\n" else "\n")
  }
 
  insertStatement <- paste0(insertStatement, "  FROM obts WHERE db_id BETWEEN ", entry$id_start, " AND ", entry$id_end)
  
  insertResult <- tryCatch(dbExecute(con, insertStatement), error = function(e) e)
  if (is(insertResult, "error")) browser()
    
  invisible(NULL)
}

splitRawColumnsForEntry <- function(con, tableDef, entry) {
  startIndex <- dbGetQuery(con, "SELECT nextval('obts_DB_ID_seq')")[[1L]]
  invisible(dbGetQuery(con, paste0("SELECT setval('obts_DB_ID_seq', ", startIndex, ", false)")))
    
  rawTableName <- paste0("obts_raw_", entry$format)
  
  insertStatement <- "INSERT INTO obts ("
  for (i in seq_len(nrow(tableDef))) {
    if (grepl("serial", tableDef$type[i])) next
    insertStatement <- paste0(insertStatement, tableDef$abbreviation[i])
    if (i != nrow(tableDef)) insertStatement <- paste0(insertStatement, ", ")
  }
  insertStatement <- paste0(insertStatement, ")\n")
  
  starts  <- if (entry$format == 1L) tableDef$start_1 else tableDef$start_2
  lengths <- if (entry$format == 1L) tableDef$length_1 else tableDef$length_2
  
  indentLength <- 9L
  insertStatement <- paste0(insertStatement, "  SELECT\n")
  for (i in seq_len(nrow(tableDef))) {
    if (grepl("serial", tableDef$type[i])) next
    
    insertStatement <- paste0(insertStatement, sprintf("%-*s", indentLength, ""),
                              "SUBSTRING(record, ", starts[i], ", ", lengths[i], ") AS ", tableDef$abbreviation[i])
    
    insertStatement <- paste0(insertStatement, if (i != nrow(tableDef)) ",\n" else "\n")
  }
  
  insertStatement <- paste0(insertStatement, "  FROM ", rawTableName, " WHERE id BETWEEN ", entry$raw_id_start, " AND ", entry$raw_id_end)
  
  endIndex <- startIndex + dbExecute(con, insertStatement)[[1L]] - 1L
  
  dbExecute(con, paste0("UPDATE info SET id_start = ", startIndex, ", id_end = ", endIndex, " WHERE file_name = '", entry$file_name, "'"))
  
  invisible(NULL)
}


splitRawColumns <- function(drv, tableDef) {
  createTablesIfNonexistent(drv, tableDef, "obts")
  
  con <- connectToDatabase(drv, "obts")
  currentEntries <- dbGetQuery(con, "SELECT * FROM info WHERE id_start = 0")
  
  if (nrow(currentEntries) > 0L) {
    for (i in seq_len(nrow(currentEntries))) {
      currentEntry <- currentEntries[i,]
      
      cat("split columns for file '", currentEntry$file_name, "'\n", sep = "")
      splitResult <- tryCatch(splitRawColumnsForEntry(con, tableDef, currentEntry), error = function(e) e)
      if (is(splitResult, "error")) {
        dbDisconnect(con)
        stop("split failed with error: ", splitResult)
      }
    }
  }
  
  dbDisconnect(con)
  
  invisible(NULL)
}

parseRawColumns <- function(drv, tableDef) {
  createTablesIfNonexistent(drv, tableDef, "obts_typed")
  
  con <- connectToDatabase(drv, "obts")
  currentEntries <- dbGetQuery(con, "SELECT * FROM info WHERE id_start != 0")
  
  definedFunctions <- FALSE
  
  if (nrow(currentEntries) > 0L) {
    for (i in seq_len(nrow(currentEntries))) {
      currentEntry <- currentEntries[i,]
      
      alreadyParsed <- dbGetQuery(con, paste0("SELECT EXISTS(SELECT 1 FROM obts_typed WHERE id = ", currentEntry$id_start, ")"))[[1L]]
      if (alreadyParsed) next
      
      if (!definedFunctions) {
        defineImportFunctions(con, tableDef)
        definedFunctions <- TRUE
      }
      
      cat("parsing columns for file '", currentEntry$file_name, "'\n", sep = "")
      parseResult <- tryCatch(parseRawColumnsForEntry(con, tableDef, currentEntry), error = function(e) e)
      if (is(parseResult, "error")) browser()
    }
    
    if (definedFunctions) dropImportFunctions(con, tableDef)
  }
  
  dbDisconnect(con)
  
  invisible(NULL)
}



if (FALSE) {

## connects to the database, creates tables and imports raw files
## change the path to point to where this file is placed
setwd("~/Repositories/dojr/import/obts")
source("import.R")

tableDef <- read.csv("tableDef.csv", stringsAsFactor = FALSE)

if (require(RPostgreSQL, quietly = TRUE) == FALSE) {
  repos <- getOption("repos")
  if (is.null(repos)) repos <- getCRANmirrors()$URL[1L]
  install.packages("PostgreSQL", repos, dependencies = TRUE)
  if (require(RPostgreSQL, quietly = TRUE) == FALSE) stop("could not install RPostgreSQL package, do so manually")
}

drv <- dbDriver("PostgreSQL")

## "input" is the folder with files ACRPTARB00.TXT, ...
updateInfoTable(drv, tableDef, "input")
updateRawTables(drv, tableDef, "input")
splitRawColumns(drv, tableDef)
parseRawColumns(drv, tableDef)



## useful to delete the current typed table to recreate it
con <- connectToDatabase(drv, "obts")

dropImportFunctions(con, tableDef)
dropTable(con, tableDef, "obts_typed")

dbDisconnect(con)


dbUnloadDriver(drv)
}
