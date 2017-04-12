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
  credentials <- read.table(file.path("dbCredentials.properties"), sep = "=", strip.white = TRUE, stringsAsFactor = FALSE)
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
  credentials <- read.table(file.path("dbCredentials.properties"), sep = "=", strip.white = TRUE, stringsAsFactor = FALSE)
  for (i in seq_len(nrow(credentials))) assign(credentials[i,1L], credentials[i,2L])
  
  ## used if we need to create a database
  dbProperties <- if (.Platform$OS.type == "unix")
    "OWNER = postgres ENCODING = 'UTF8' LC_COLLATE = 'C' LC_CTYPE = 'C' TABLESPACE = pg_default CONNECTION LIMIT = -1"
  else
    "OWNER = postgres ENCODING = 'UTF8' LC_COLLATE = 'English_United States.1252' LC_CTYPE = 'English_United States.1252' TABLESPACE = pg_default CONNECTION LIMIT = -1"
  
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
        dbExecute(con.su, paste0("CREATE USER ", roleString))
      } else {
        dbExecute(con.su, paste0("ALTER USER ", roleString))
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
  
  if (is(connectionResult, "error")) stop(connectionResult)
  connectionResult
}

dbExistsType <- function(con, name)
  dbGetQuery(con, paste0("SELECT EXISTS(SELECT 1 FROM pg_type WHERE typname = '", name, "')"))[[1L]]

parseEnumType <- function(x) {
  enumSpec <- strsplit(strsplit(x, "; ")[[1L]], " = ")
  values <- sapply(enumSpec, function(x) x[2L])
  names(values) <- sapply(enumSpec, function(y) sub("'(.*)'", "\\1", y[1L]))
  values
}

isIntegerType <- function(x) grepl("int$", x)
isSequenceType <- function(x) grepl("serial", x)
isEnumType <- function(x) grepl(";", x)

## used to reset some stuff for testing purposes
dropTable <- function(con, tableDef, tableNames)
{
  if ("macr_typed" %in% tableNames) {
    if (dbExistsTable(con, "macr_typed")) dbExecute(con, "DROP TABLE macr_typed")
    
    for (i in seq_len(nrow(tableDef))) {
      if (!isEnumType(tableDef$type[i])) next
      typeName <- paste0("macr_", tableDef$full_name[i])
      if (dbExistsType(con, typeName)) dbExecute(con, paste0("DROP TYPE ", typeName))
    }
  }
  if ("macr" %in% tableNames) {
    if (dbExistsTable(con, "macr")) dbExecute(con, "DROP TABLE macr")
    
    if (dbExistsTable(con, "macr_info")) dbExecute(con, "UPDATE macr_info SET id_start = 0, id_end = 0")
  }
  if ("macr_raw" %in% tableNames) {
    if (dbExistsTable(con, "macr_raw")) dbExecute(con, "DROP TABLE macr_raw")
    if (dbExistsTable(con, "macr_info")) dbExecute(con, "UPDATE macr_info SET raw_id_start = 0, raw_id_end = 0")
  }
    
  if ("macr_info" %in% tableNames && dbExistsTable(con, "macr_info")) dbExecute(con, "DROP TABLE macr_info")
  
  invisible(NULL)
}

resetDatabase <- function(drv, tableDef)
{
  con <- connectToDatabase(drv, "cjsc")
  
  dropTable(con, tableDef, "macr_typed")
  dropImportFunctions(con, tableDef)
  
  dropTable(con, tableDef, c("macr", "macr_raw"))
  dbExecute(con, "DELETE FROM macr_info")
  
  dbDisconnect(con)
  
  invisible(NULL)
}


createTablesIfNonexistent <- function(drv, tableDef, tableNames)
{
  con <- connectToDatabase(drv, "cjsc")
  
  if ("macr_info" %in% tableNames && !dbExistsTable(con, "macr_info")) {
    dbExecute(con,
      "CREATE TABLE macr_info (
         file_name      varchar(64),
         size           bigint,
         timestamp      timestamp,
         hash           varchar(32),
         id_start       bigint,
         id_end         bigint,
         raw_id_start   bigint,
         raw_id_end     bigint
       )")
  }
  if ("macr_raw" %in% tableNames && !dbExistsTable(con, "macr_raw")) {
    dbExecute(con,
      "CREATE TABLE macr_raw (
         id         bigserial,
         record     character(84)
       )")
  }
  if ("macr" %in% tableNames && !dbExistsTable(con, "macr")) {
    createString <- "CREATE TABLE macr (\n"
    nameLength <- 2L + max(nchar(tableDef$abbreviation))
    
    for (i in seq_len(nrow(tableDef))) {
      if (isSequenceType(tableDef$type[i])) 
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$abbreviation[i]), tableDef$type[i], " PRIMARY KEY")
      else
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$abbreviation[i]), "character(", tableDef$length[i], ")")
      
      createString <- paste0(createString, if (i != nrow(tableDef)) ",\n" else "\n")
    }
    createString <- paste0(createString, ")")
    
    dbExecute(con, createString)
  }
    
  if ("macr_typed" %in% tableNames && !dbExistsTable(con, "macr_typed")) {
    ## can probably move these typedefs into a schema to help with namespace stuff
    
    for (i in seq_len(nrow(tableDef))) {
      if (isEnumType(tableDef$type[i])) {
        enumSpec <- parseEnumType(tableDef$type[i])
        
        name <- paste0("macr_", tableDef$full_name[i])
        if (dbExistsType(con, name)) {
          warning("type '", name, "' already exists, dropping")
          dbExecute(con, paste0("DROP TYPE ", name))
        }
        createString <- paste0("CREATE TYPE ", name, " AS ENUM ('", paste0(enumSpec, collapse = "', '"), "')")
        dbExecute(con, createString)
      }
    }
    
    ## this is hacky, as it references the macr key to create a foreign constraint
    createString <- "CREATE TABLE macr_typed (\n"
    nameLength <- 2L + max(nchar(tableDef$full_name))
    for (i in seq_len(nrow(tableDef))) {
      if (isSequenceType(tableDef$type[i])) {
        keyType <- sub("serial", "int", tableDef$type[i])
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$full_name[i]), keyType, " REFERENCES macr(db_id)")
      } else if (!isEnumType(tableDef$type[i])) {
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$full_name[i]), tableDef$type[i])
      } else {
        createString <- paste0(createString, sprintf("  %-*s", nameLength, tableDef$full_name[i]), "macr_", tableDef$full_name[i])
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
    if (!isEnumType(tableDef$type[i])) next
    
    enumSpec <- parseEnumType(tableDef$type[i])
    enumName <- tableDef$full_name[i]
    
    fieldLength <- max(tableDef[i,colnames(tableDef)[grepl("length", colnames(tableDef))]])
    createString <- paste0("CREATE OR REPLACE FUNCTION string_to_", enumName, "(character(", fieldLength, ")) RETURNS macr_", enumName, " AS $$\n",
      "  SELECT CASE\n")
    hasElse <- FALSE
    for (j in seq_along(enumSpec)) {
      if (names(enumSpec)[j] != "") {
        createString <- paste0(createString, "    WHEN $1 = '", names(enumSpec)[j], "' THEN CAST('", enumSpec[j], "' AS macr_", enumName, ")\n")
      } else {
        createString <- paste0(createString, "    ELSE CAST('", enumSpec[j], "' AS macr_", enumName, ")\n")
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
    if (!isEnumType(tableDef$type[i])) next
    
    functionName <- paste0("string_to_", tableDef$full_name[i])
    # field length is used when creating, but not dropping
    #fieldLength <- max(tableDef[i,colnames(tableDef)[grepl("length", colnames(tableDef))]])
    functionArgumentType <- paste0("character")
    if (dbExistsFunction(con, functionName, functionArgumentType))
      dbExecute(con, paste0("DROP FUNCTION ", functionName, "(", functionArgumentType, ")"))
  }
  invisible(NULL)
}


insertFileIntoInfoTable <- function(con, inputPath, fileName) {
  fileInfo <- file.info(file.path(inputPath, fileName))
  hash     <- tools::md5sum(file.path(inputPath, fileName))
  
  insertStatement <- paste0(
    "INSERT INTO macr_info VALUES
      ('", paste(fileName, fileInfo$size, format(fileInfo$mtime, format = "%F %X %z"), hash,
                 0, 0, 0, 0, sep = "', '"), "')")
  dbExecute(con, insertStatement)
  
  invisible(NULL)
}

deleteDataRowsForEntry <- function(con, entry) {
  if (entry$id_start != 0L) {
    dbExecute(con, paste0("DELETE FROM macr_typed WHERE db_id BETWEEN ", entry$id_start, " AND ", entry$id_end))
    dbExecute(con, paste0("DELETE FROM macr WHERE db_id BETWEEN ", entry$id_start, " AND ", entry$id_end))
  }
  
  if (entry$raw_id_start != 0L) {
    dbExecute(con, paste0("DELETE FROM macr_raw WHERE id BETWEEN ", entry$raw_id_start, " AND ", entry$raw_id_end))
  }
  invisible(NULL)
}

updateInInfoTableAndDeleteDataRows <- function(con, inputPath, fileName, currentEntry) {
  deleteDataRowsForEntry(con, currentEntry)
  
  fileInfo <- file.info(file.path(inputPath, fileName))
  hash     <- tools:md5sum(file.path(inputPath, fileName))
  
  updateStatement <- paste0("UPDATE macr_info SET ",
    "timestamp = '", format(fileInfo$mtime, format = "%F %X %z"), "', ",
    "hash = '", hash, "', ",
    "size = '", fileInfo$size, "', ",
    "id_start = 0, id_end = 0, raw_id_start = 0, raw_id_end = 0 ",
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
  Sys.setenv(LC_ALL = "C")
  if (.Platform$OS.type == "unix") {
    system2("tr", args = "'\\000\\377' '  '",
            stdin = inputFile,
            stdout = tempFile)
  } else {
    system2("tr", args = "'\\000\\377' '  '",
            stdin = inputFile,
            stdout = tempFile)
    system2("chmod", c("644", basename(tempFile)))
  }
  lineLength <- nchar(readLines(tempFile, n = 1L))
  
  #inputFormat <- switch(as.character(lineLength), "176" = 1L, "184" = 2L, NULL)
  #if (is.null(inputFormat)) {
  #  unlink(tempFile)
  #  stop("unrecognized format for file: ", fileName)
  #}
  #rawTableName <- paste0("obts_raw_", inputFormat)
  rawTableName <- "macr_raw"
  
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
    "UPDATE macr_info SET
       raw_id_start = ", rawStartIndex, ",
       raw_id_end = ", rawEndIndex, "
       WHERE 
       file_name = '", fileName, "'")
  dbExecute(con, updateStatement)
}

updateInfoTable <- function(drv, tableDef, inputPath) {
  createTablesIfNonexistent(drv, tableDef, "macr_info")
  
  inputFiles <- list.files(inputPath)
  
  con <- connectToDatabase(drv, "cjsc")
  currentEntries <- dbGetQuery(con, "SELECT * FROM macr_info")
  
  for (fileName in inputFiles) {
    if (!(fileName %in% currentEntries$file_name)) {
      cat("inserting file '", fileName, "' into info table\n", sep = "")
      
      insertFileIntoInfoTable(con, inputPath, fileName)
      next
    }
    
    currentEntry <- currentEntries[currentEntries$file_name == fileName,]
    
    fileInfo <- file.info(file.path(inputPath, fileName))
    if (fileInfo$mtime == currentEntry$timestamp && fileInfo$size == currentEntry$size) next
  
    ## only timestamps differ, check the hash
    if (fileInfo$size == currentEntry$size) {
      hash <- tools::md5sum(file.path(inputPath, inputFile))
      if (hash == currentEntry$hash) {
        cat("updating timestamp for file '", fileName, "' in info table\n", sep = "")
        
        dbExecute(con, paste0("UPDATE macr_info SET timestamp = '", format(fileInfo$mtime, format = "%F %X %z"),
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
      dbExecute(con, paste0("DELETE FROM macr_info WHERE file_name = '", extraFileEntry$file_name, "'"))
    }
  }
  
  dbDisconnect(con)
  
  invisible(NULL)
}

updateRawTables <- function(drv, tableDef, inputPath) {
  createTablesIfNonexistent(drv, tableDef, "macr_raw")
  
  con <- connectToDatabase(drv, "cjsc")
  currentEntries <- dbGetQuery(con, "SELECT * FROM macr_info WHERE raw_id_start = 0")
  
  if (nrow(currentEntries) > 0L) {
    con.su <- connectToDatabase(drv, "cjsc", TRUE)
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
  insertStatement <- "INSERT INTO macr_typed ("
  for (i in seq_len(nrow(tableDef))) {
    insertStatement <- paste0(insertStatement, tableDef$full_name[i])
    if (i != nrow(tableDef)) insertStatement <- paste0(insertStatement, ", ")
  }
  insertStatement <- paste0(insertStatement, ")\n")
  
  indentLength <- 9L
  insertStatement <- paste0(insertStatement, "  SELECT\n")
  for (i in seq_len(nrow(tableDef))) {
    insertStatement <- paste0(insertStatement, sprintf("%-*s", indentLength, ""))
    
    if (isIntegerType(tableDef$type[i])) {
      insertStatement <- paste0(insertStatement, "string_to_int(macr.", tableDef$abbreviation[i], ") AS ", tableDef$full_name[i])
    } else if (tableDef$type[i] == "date") {
      insertStatement <- paste0(insertStatement, "string_to_date(macr.", tableDef$abbreviation[i], ", 'YYYYMMDD') AS ", tableDef$full_name[i])
    } else if (isEnumType(tableDef$type[i])) {
      insertStatement <- paste0(insertStatement, "string_to_", tableDef$full_name[i], "(macr.", tableDef$abbreviation[i], ") AS ", tableDef$full_name[i])
    } else {
      insertStatement <- paste0(insertStatement, "macr.", tableDef$abbreviation[i], " AS ", tableDef$full_name[i])
    }
   
    insertStatement <- paste0(insertStatement, if (i != nrow(tableDef)) ",\n" else "\n")
  }
 
  insertStatement <- paste0(insertStatement, "  FROM macr WHERE db_id BETWEEN ", entry$id_start, " AND ", entry$id_end)
  
  insertResult <- tryCatch(dbExecute(con, insertStatement), error = function(e) e)
  if (is(insertResult, "error")) browser()
    
  invisible(NULL)
}

splitRawColumnsForEntry <- function(con, tableDef, entry) {
  startIndex <- dbGetQuery(con, "SELECT nextval('macr_DB_ID_seq')")[[1L]]
  invisible(dbGetQuery(con, paste0("SELECT setval('macr_DB_ID_seq', ", startIndex, ", false)")))
    
  rawTableName <- "macr_raw"
  
  insertStatement <- "INSERT INTO macr ("
  for (i in seq_len(nrow(tableDef))) {
    if (isSequenceType(tableDef$type[i])) next
    insertStatement <- paste0(insertStatement, tableDef$abbreviation[i])
    if (i != nrow(tableDef)) insertStatement <- paste0(insertStatement, ", ")
  }
  insertStatement <- paste0(insertStatement, ")\n")
  
  starts  <- tableDef$start 
  lengths <- tableDef$length
  
  indentLength <- 9L
  insertStatement <- paste0(insertStatement, "  SELECT\n")
  for (i in seq_len(nrow(tableDef))) {
    if (isSequenceType(tableDef$type[i])) next
    
    insertStatement <- paste0(insertStatement, sprintf("%-*s", indentLength, ""),
                              "SUBSTRING(record, ", starts[i], ", ", lengths[i], ") AS ", tableDef$abbreviation[i])
    
    insertStatement <- paste0(insertStatement, if (i != nrow(tableDef)) ",\n" else "\n")
  }
  
  insertStatement <- paste0(insertStatement, "  FROM ", rawTableName, " WHERE id BETWEEN ", entry$raw_id_start, " AND ", entry$raw_id_end)
  
  endIndex <- startIndex + dbExecute(con, insertStatement)[[1L]] - 1L
  
  dbExecute(con, paste0("UPDATE macr_info SET id_start = ", startIndex, ", id_end = ", endIndex, " WHERE file_name = '", entry$file_name, "'"))
  
  invisible(NULL)
}


splitRawColumns <- function(drv, tableDef) {
  createTablesIfNonexistent(drv, tableDef, "macr")
  
  con <- connectToDatabase(drv, "cjsc")
  currentEntries <- dbGetQuery(con, "SELECT * FROM macr_info WHERE id_start = 0")
  
  if (nrow(currentEntries) > 0L) {
    for (i in seq_len(nrow(currentEntries))) {
      currentEntry <- currentEntries[i,]
      
      cat("spliting columns for file '", currentEntry$file_name, "'\n", sep = "")
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
  createTablesIfNonexistent(drv, tableDef, "macr_typed")
  
  con <- connectToDatabase(drv, "cjsc")
  currentEntries <- dbGetQuery(con, "SELECT * FROM macr_info WHERE id_start != 0")
  
  definedFunctions <- FALSE
  
  if (nrow(currentEntries) > 0L) {
    for (i in seq_len(nrow(currentEntries))) {
      currentEntry <- currentEntries[i,]
      
      alreadyParsed <- dbGetQuery(con, paste0("SELECT EXISTS(SELECT 1 FROM macr_typed WHERE db_id = ", currentEntry$id_start, ")"))[[1L]]
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

checkColumns <- function(con, tableDef, columnNames = NULL)
{
  dfMismatch <- function(old, new, enumSpec = NULL) {
    if (!is.null(enumSpec)) {
      names.old <- names(enumSpec)
      count.old <- as.vector(old[match(names.old, names(old))])
      if (any(!(names(old) %in% names.old))) {
        extraValues <- !(names(old) %in% names.old)
        names.old <- c(names.old, names(old)[extraValues])
        count.old <- c(count.old, as.vector(old[extraValues]))
      }
      if (anyNA(count.old)) count.old[is.na(count.old)] <- 0L
      
      names.new <- unname(enumSpec)
      count.new <- as.vector(new[match(names.new, names(new))])
      if (any(!(names(new) %in% names.new))) {
        extraValues <- !(names(new) %in% names.new)
        names.new <- c(names.new, names(new)[extraValues])
        count.new <- c(count.new, as.vector(new[extraValues]))
      }
      if (anyNA(count.new)) count.new[is.na(count.new)] <- 0L
    } else {
      names.old <- names(old)
      count.old <- as.vector(old)
      names.new <- names(new)
      count.new <- as.vector(new)
    }
    length.old <- length(names.old)
    length.new <- length(names.new)
     
    data.frame(old = c(names.old, rep(NA_character_, max(length.new - length.old, 0L))),
               count.old = c(count.old, rep(NA_integer_, max(length.new - length.old, 0L))),
               new = c(names.new, rep(NA_character_, max(length.old - length.new, 0L))),
               count.new = c(count.new, rep(NA_integer_, max(length.old - length.new, 0L))))
  }
  
  columnIndexSet <- if (is.null(columnNames)) seq_len(nrow(tableDef)) else match(columnNames, tableDef$full_name)
  
  for (i in columnIndexSet) {
    if (isSequenceType(tableDef$type[i])) next
    if (isEnumType(tableDef$type[i])) {
      ## will fail if multiple categories weren't re-coded
      old <- table(dbGetQuery(con, paste0("SELECT ", tableDef$abbreviation[i], " FROM macr"))[[1L]], useNA = "ifany")
      new <- table(dbGetQuery(con, paste0("SELECT ", tableDef$full_name[i], " FROM macr_typed"))[[1L]], useNA = "ifany")
      
      enumSpec <- parseEnumType(tableDef$type[i])
      tableMismatch <- dfMismatch(old, new, enumSpec)
      if (length(old) != length(new) || any(sort(old) != sort(new)) ||
          (anyNA(tableMismatch$new) && !grepl("^\\s*$", as.character(tableMismatch$old[is.na(tableMismatch$new)])))) {
        cat("mismatch in column ", tableDef$full_name[i], "(", tableDef$abbreviation[i], "):\n", sep = "")
        print(tableMismatch)
      }
    } else if (isIntegerType(tableDef$type[i])) {
      ## this should only fail if there are more than one codes which fail to parse as an int
      old <- table(dbGetQuery(con, paste0("SELECT ", tableDef$abbreviation[i], " FROM macr"))[[1L]], useNA = "ifany")
      new <- table(dbGetQuery(con, paste0("SELECT ", tableDef$full_name[i], " FROM macr_typed"))[[1L]], useNA = "ifany")
      
      if (length(old) != length(new) || any(sort(old) != sort(new))) {
        cat("mismatch in column ", tableDef$full_name[i], "(", tableDef$abbreviation[i], "):\n", sep = "")
        print(dfMismatch(old, new))
      }
    } else if (tableDef$type[i] == "date") {
      join <- dbGetQuery(con, paste0("SELECT db_id, ", tableDef$abbreviation[i], ", ", tableDef$full_name[i],
                                     " FROM macr_typed JOIN macr ON macr_typed.db_id = macr.db_id WHERE ", tableDef$full_name[i], " IS NULL"))
      join[[2L]] <- trimws(join[[2L]])
      tryResult <- tryCatch(badDays <- sapply(join[[2L]], function(x) if (nchar(x) == 6L) (substr(x, 5L, 6L) != "00") else (substr(x, 7L, 8L) != "00")), error = function(e) e)
      if (is(tryResult, "error")) browser()
      
      cat(sum(badDays), " dates failed to parse in column ", tableDef$full_name[i], "(", tableDef$abbreviation[i], ") that were not 00\n", sep = "")
    }
  }
}

tableDef <- read.csv(file.path("datasets", "macr", "tableDef.csv"), stringsAsFactor = FALSE)

if (require(RPostgreSQL, quietly = TRUE) == FALSE) {
  repos <- getOption("repos")
  if (is.null(repos) || is.na(repos["URL"])) repos <- getCRANMirrors()[1L,]
  
  install.packages("RPostgreSQL", repos = repos, dependencies = TRUE)
  if (require(RPostgreSQL, quietly = TRUE) == FALSE) stop("could not install RPostgreSQL package, do so manually")
  rm(repos)
}

drv <- dbDriver("PostgreSQL")

## "input" is the folder with files ACRPTARB00.TXT, ...
updateInfoTable(drv, tableDef, file.path("datasets", "macr", "raw"))
updateRawTables(drv, tableDef, file.path("datasets", "macr", "raw"))
splitRawColumns(drv, tableDef)
parseRawColumns(drv, tableDef)

dbUnloadDriver(drv)

if (FALSE) {

## connects to the database, creates tables and imports raw files
## change the path to point to where this file is placed
setwd("~/Repositories/dojr/ojdr")
source(file.path("datasets", "macr", "import.R"))

tableDef <- read.csv(file.path("datasets", "macr", "tableDef.csv"), stringsAsFactor = FALSE)

if (require(RPostgreSQL, quietly = TRUE) == FALSE) {
  repos <- getOption("repos")
  if (is.null(repos) || is.na(repos["URL"])) repos <- getCRANmirrors()[1L,]
  
  install.packages("RPostgreSQL", repos = repos, dependencies = TRUE)
  if (require(RPostgreSQL, quietly = TRUE) == FALSE) stop("could not install RPostgreSQL package, do so manually")
  rm(repos)
}

drv <- dbDriver("PostgreSQL")

## "input" is the folder with files ACRPTARB00.TXT, ...
updateInfoTable(drv, tableDef, file.path("datasets", "macr", "raw"))
updateRawTables(drv, tableDef, file.path("datasets", "macr", "raw"))
splitRawColumns(drv, tableDef)
parseRawColumns(drv, tableDef)



## useful to delete the current typed table to recreate it
con <- connectToDatabase(drv, "cjsc")

dropImportFunctions(con, tableDef)
dropTable(con, tableDef, "macr_typed")

dbDisconnect(con)


## validate import
con <- connectToDatabase(drv, "cjsc")

checkColumns(con, tableDef)

dbDisconnect(con)


## export to file
con <- connectToDatabase(drv, "cjsc")

macr <- dbGetQuery(con, "SELECT * FROM macr_typed")

dbDisconnect(con)

## enum types in obts
for (variableName in c("cii_record_type", "pdr_id", "gender", "race", "deceased", "last_step", "arrest_bypass", "arrest_converted_data", "arrest_offense_level", "arrest_disposition_type", "prior_record_code", "court_level", "court_bypass", "court_disposition_type", "court_proceeding_type", "sentence_type", "sentence", "court_charge_type"))
  obts[[variableName]] <- as.factor(obts[[variableName]])

## integer types that are really categorical
for (variableName in c("arresting_agency", "multiple_arrest_charges", "arrest_multiple_dispositions", "arrest_offense", "arrest_summary_code", "court_converted_data", "court_judicial_district", "court_multiple_dispositions", "court_multiple_disposition_charges", "court_disposition_offense", "court_qualifier", "court_summary_code"))
  obts[[variableName]] <- as.factor(obts[[variableName]])


save(obts, file = file.path("..", "..", "common", "data", "obts.Rdata"))

dbUnloadDriver(drv)

}
