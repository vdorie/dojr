## used to extract cjis codes from the OBTS manual
## requires some pre-work to extract just the relevant tables from the pdf
## and uses the utility 'pdftotext'

## run from dojr/inventory/obts by source("src/cjisCodes.R")
## assumes cjisCodes.txt exists in dojr/inventory/obts

## run these from the command line
# pdftotext -layout -f 63 -l 197 OBTS_TechManual_1982-2014.pdf codes.txt
# grep -e '^[[:alnum:]]\+' codes.txt > codes2.txt
## OS X's grep reaallly really sucks
# grep -E -e '^[^C][^J][^I][^S]' codes2.txt > codes.txt
# rm codes2.txt

codes <- readLines("cjisCodes.txt")
prefix <- character(length(codes))
suffix <- character(length(codes))

## different pages had different amounts of space in the table, so we have to search to find the columns
## could easily do this using greps/seds over the vector, but
##   a) a few don't have a code type
##   b) this lets me check to see that all the code types are being caught
for (i in seq_along(codes)) {
  matchIndex <- regexpr("[[:space:]](VC|PC|FA|LC|BP|HN|WI|SH|MV|IC|CI|CP|FG|HS|FC|UI|CC|PU|PR|RT|GC|EL|EC|US|CA|WC|ZZ|UF)[[:space:]]", codes[i])
  if (matchIndex == -1L) {
    ## optimistically assumes that we can use the previous row
    ## luckily, this is legit but may change with future versions of the manual
    cat("no match on row ", i, ":\n  ", codes[i], "\n", sep = "")
    prefix[i] <- substr(codes[i], 1L, nchar(prefix[i - 1L]))
    suffix[i] <- substr(codes[i], nchar(prefix[i - 1L]) + 1L, nchar(codes[i]))
  } else {
    prefix[i] <- substr(codes[i], 1L, matchIndex)
    suffix[i] <- substr(codes[i], matchIndex + 1L, nchar(codes[i]))
  }
}

cjisCode <- substr(prefix, 1L, 5L)
statuteCode <- sub("^[[:space:]]*([^[:space:]].*[^[:space:]])[[:space:]]*$", "\\1",
                    substr(prefix, 6L, nchar(prefix)))

hierarchyIndices <- regexpr("[[:digit:]]+$", suffix)
hierarchy <- as.integer(substr(suffix, hierarchyIndices, nchar(suffix)))
suffix <- substr(suffix, 1L, hierarchyIndices - 1L)

summaryCodeIndices <- regexpr("([[:digit:]]{2})[[:space:]]+$", suffix)
summaryCode <- as.integer(substr(suffix, summaryCodeIndices, summaryCodeIndices + 1L))
suffix <- substr(suffix, 1L, summaryCodeIndices - 1L)

tocIndices <- regexpr("(F|M|N|I|S)[[:space:]]*$", suffix)
toc <- substr(suffix, tocIndices, tocIndices)
suffix <- substr(suffix, 1L, tocIndices - 1L)

codeType <- substr(suffix, 1L, 2L)
codeType[codeType == "  "] <- NA

literal <- sub("^[[:space:]]*([^[:space:]].*[^[:space:]])[[:space:]]*$", "\\1",
                    substr(suffix, 3L, nchar(suffix)))

cjisCodes <- data.frame(cjis_code = cjisCode, statue_code = statuteCode, code_type = codeType,
                        literal = literal, toc = toc, summary_code = summaryCode, hierarchy = hierarchy,
                        stringsAsFactors = FALSE)
cjisCodes$code_type <- as.factor(cjisCodes$code_type)
cjisCodes$toc <- as.factor(cjisCodes$toc)

write.csv(cjisCodes,
          file.path("..", "..", "common", "data", "cjisCodes.csv"),
          row.names = FALSE,
          quote = c(2L, 4L))
