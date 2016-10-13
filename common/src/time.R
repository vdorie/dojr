## for Date objects, gives (end - start) in years
## and is suitable for calculating an "age" from 
## a birth date 
getTimeDifferenceInYears <- function(end, start) {
  end   <- as.POSIXlt(end)
  start <- as.POSIXlt(start)
  
  age <- end$year - start$year
  
  ifelse(end$mon < start$mon |
         (end$mon == start$mon & end$mday < start$mday),
         age - 1L, age)
}
