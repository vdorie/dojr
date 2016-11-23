dataPath <- file.path("..", "common", "data")

imgPath <- file.path(datasetDir, "img")
txtPath <- file.path(datasetDir, "txt")

commonSrcPath <- file.path("..", "common", "src")

dataSrcPath <- file.path(datasetDir, "src")

dir.create(imgPath, showWarnings = FALSE, recursive = TRUE)
dir.create(txtPath, showWarnings = FALSE, recursive = TRUE)

tables <- read.csv(file.path(datasetDir, "tables.csv"), stringsAsFactors = FALSE)

mainTable <- tables$name[which.max(tables$main_table == 1L)]

dataLoaded <- rep(FALSE, nrow(tables))
names(dataLoaded) <- tables$name

loadData <- function(tableName = mainTable, path = ".") {
  tableIndex <- which.max(tables$name == tableName)
  if (!.GlobalEnv$dataLoaded[[tableName]]) {
    dataFilePath <- file.path(path, dataPath, tables$file[tableIndex])
    if (grepl("\\.csv$", tables$file[tableIndex])) {
      .GlobalEnv[[tables$object[tableIndex]]] <- read.csv(dataFilePath, stringsAsFactors = FALSE)
    } else if (grepl("\\.Rdata$", tables$file[tableIndex])) {
      load(dataFilePath, envir = .GlobalEnv)
    } else if (grepl("\\.dta$", tables$file[tableIndex])) {
      require(foreign)
      .GlobalEnv[[tables$object[tableIndex]]] <- read.dta(dataFilePath)
    } else {
      stop("unrecognized file format: '", tables$file[tableIndex], "'")
    }
    if (!is.na(tables$on_load[tableIndex]) && tables$on_load[tableIndex] != "")
      eval(parse(text = tables$on_load[tableIndex]), .GlobalEnv)
  }
  .GlobalEnv$dataLoaded[[tableName]] <- TRUE
  
  invisible(get(tables$object[tableIndex]))
}

unloadData <- function(tableName) {
  tableIndex <- which.max(tables$name == tableName)
  if (.GlobalEnv$dataLoaded[[tableName]]) {
    if (exists(tableName, envir = .GlobalEnv)) rm(tableName, envir = .GlobalEnv)
    .GlobalEnv$dataLoaded[[tableName]] <- FALSE
  }
  invisible(NULL)
}

## load some utilities
source(file.path(commonSrcPath, "knitr.R"))
source(file.path(commonSrcPath, "util.R"))

## inventory specific
srcFiles <- list.files(srcPath, "*.R", full.names = TRUE)
srcFiles <- srcFiles[grepl("src/(?!setup).*\\.R", srcFiles, perl = TRUE)]
for (srcFile in srcFiles) source(srcFile, local = TRUE)
rm(srcFiles, srcFiles)

## data-set specific
if (file.exists(file.path(dataSrcPath, "init.R"))) source(file.path(dataSrcPath, "init.R"))
