context("rsupp calcRisk")

testData <- data.frame(gender = c("male", "male", "male", "male", "female"),
                       age_group = c("10 to 17", "10 to 17", "18 to 19", "18 to 19", "20 to 29"),
                       race = c("white", "white", "white", "white", "black"),
                       offense_level = c("misdemeanor", "misdemeanor", "misdemeanor", "felony", "misdemeanor"))

test_that("fails with invalid inputs", {
  expect_error(calcRisk(testData, keyVars = "not-a-col"))
  expect_error(calcRisk(testData, divVar = "not-a-col"))
  expect_error(calcRisk(testData, divVar = "gender"))
  expect_error(calcRisk(testData, risk.f = "not-a-function"))
  expect_error(calcRisk(testData, strataVars = "not-a-col"))
  expect_error(calcRisk(testData, keyVars = c("gender", "age_group", "race"), strataVars = "gender"))
  
  testData$temp <- LETTERS[nrow(testData)]
  expect_error(calcRisk(testData))
  testData$temp <- NULL
})

test_that("computes risk correctly", {
  ## k-anon
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race")),
               c(2, 2, 2, 2, 1))
  ## l-diversity
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), divVar = "offense_level",
                        risk.f = function(x) as.double(sum(x > 0))),
               c(1, 1, 2, 2, 1))
  ## soft ordering
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), divVar = "offense_level",
                        risk.f = function(x) as.double(x["misdemeanor"] > 0L)),
               c(1, 1, 1, 1, 1))
  ## percent
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), divVar = "offense_level",
                        risk.f = function(x) x["misdemeanor"] / sum(x)),
               c(1, 1, 0.5, 0.5, 1))
})

test_that("works with multiple strata", {
  testData$strata <- factor(c(1L, 1L, 1L, 2L, 2L), labels = c("g1", "g2"))
  
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), strataVars = "strata"),
               c(2, 2, 1, 1, 1))
})

test_that("returns the correct answer with NAs present", {
  testData <- as.data.frame(matrix(c(
      "male", "10 to 17", "white",       "felony",
      "male", "10 to 17", "white",       "felony",
      "male", "10 to 17", NA_character_, "misdemeanor",
      "male", "10 to 17", "black",       "felony"),
    byrow = TRUE, nrow = 4L,
    dimnames = list(NULL, c("gender", "age_group", "race", "offense_level"))))
  
  keyVars  <- c("gender", "age_group", "race")
  divVar   <- "offense_level"
  risk.f.p <- function(x) x['misdemeanor'] / sum(x)
  risk.f.l <- function(x) sum(x > 0)
  ## if we just look at the first 3 rows, the NA can only be part of the white group
  expect_equal(calcRisk(testData[seq_len(3L),], keyVars = keyVars),
               rep_len(3, 3L))
  expect_equal(calcRisk(testData[seq_len(3L),], keyVars = keyVars, divVar = divVar, risk.f = risk.f.p),
               rep_len(1 / 3, 3L))
  expect_equal(calcRisk(testData[seq_len(3L),], keyVars = keyVars, divVar = divVar, risk.f = risk.f.l),
               rep_len(2, 3L))
  
  ## if we look at all the data, the NA can be white or black
  expect_equal(calcRisk(testData, keyVars = keyVars),
               c(3, 3, 2, 2))
  ## conversely, the minimum % misd goes to the first group, since it'll only be 1/3
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.p),
               c(rep_len(1 / 3, 3L), 1 / 2))
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.l),
               rep_len(2, 4L))
  
  testData <- as.data.frame(matrix(c(
      "male",   "10 to 17", "white", "felony",
      "male",   "10 to 17", "white", "felony",
      "male",   "18 to 19", "white", "misdemeanor",
      "male",   "10 to 17", "white", "felony"),
    byrow = TRUE, nrow = 4L,
    dimnames = list(NULL, c("gender", "age_group", "race", "offense_level"))))
  testData$age_group[seq_len(4L)] <- NA
  
  ## all are NA, should match the size of the NA group
  expect_equal(calcRisk(testData, keyVars = keyVars),
               rep_len(4, 4L))
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.p),
               rep_len(1 / 4, 4L))
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.l),
               rep_len(2, 4L))
  
  testData <- as.data.frame(matrix(c(
      "male",   "10 to 17", "white", "felony",
      "male",   "10 to 17", "white", "felony",
      "male",   "18 to 19", "white", "misdemeanor",
      "male",   "10 to 17", "white", "felony",
      "female", "10 to 17", "white", "misdemeanor"),
    byrow = TRUE, nrow = 5L,
    dimnames = list(NULL, c("gender", "age_group", "race", "offense_level"))))
  testData$age_group[seq_len(3L)] <- NA
  
  ## NA are again lumped together with another obs
  expect_equal(calcRisk(testData, keyVars = keyVars),
               c(rep_len(4, 4L), 1))
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.p),
               c(rep_len(1 / 4, 4L), 1))
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.l),
               c(rep_len(2, 4L), 1))
  
  testData <- as.data.frame(matrix(c(
      "male",   "10 to 17", "white", "felony",
      "male",   "10 to 17", "white", "felony",
      "male",   "18 to 19", "white", "misdemeanor",
      "male",   "10 to 17", "white", "felony",
      "male",   "10 to 17", "white", "felony",
      "female", "10 to 17", "white", "misdemeanor"),
    byrow = TRUE, nrow = 6L,
    dimnames = list(NULL, c("gender", "age_group", "race", "offense_level"))))
  testData$age_group[seq_len(4L)] <- NA
  testData$gender[1L] <- NA
  
  ## first row matches all rows, but min riks is 2 due to row 6
  ## 2 through 5 match 1 through 5, risk is 5
  expect_equal(calcRisk(testData, keyVars = keyVars),
               c(2, rep_len(5, 4L), 2))
  ## same as above, but min for first row is now supplied by rows 1 through 5
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.p),
               c(rep_len(1 / 5, 5L), 0.5))
  expect_equal(calcRisk(testData, keyVars = keyVars, divVar = divVar, risk.f = risk.f.l),
               rep_len(2, 6L))
  
  # These tables can be difficult to parse, so consider just the first row. It matches
  # anything in the first two columns and white/NA in the last, that includes:
  #     male/30 to 39: 10
  #     male/40 to 69:  7
  #   female/30 to 39:  6
  #   female/40 to 69:  3
  #
  # But because there are no females left in the data, we rules those out and have
  # the answer be the minimum of those where the marginals are still present, i.e. 7
  
  # For the second row:
  #   white: 7
  #   black: 8
  # but here things are trickier. There are no complete instances of male/40 to 69/white,
  # but there are of black, so the black number takes precendence

  testData <- data.frame(matrix(c(
      NA,     NA,         "white", 
      "male", "40 to 69", NA,
      NA,     NA,         NA,
      "male", NA,         NA,
      NA,    "30 to 39",  NA,
      "male", "30 to 39", "black",
      NA,     "30 to 39", "white",
      "male", NA,         NA,
      "male", "40 to 69", "black",
      "male", "30 to 39", NA,
      "male", NA,         NA,
      NA,     "30 to 39", "black",
      NA,     NA,         "white",
      "male", "40 to 69", "black",
      "male", "40 to 69", "black",
      NA,     "30 to 39", "white"),
    byrow = TRUE, ncol = 3L,
    dimnames = list(NULL, c("gender", "age_group", "race"))))

  levels(testData$gender) <- c("male", "female")
  cbind(testData, risk = calcRisk(testData))

  
  expect_equal(calcRisk(testData, keyVars = keyVars)[seq_len(2L)],
               c(7, 8))
  
  ## marginal counts for missing columns (again)
  testData <- data.frame(matrix(c(
      "male", NA,         NA,
      "male", NA,         "white",
      "male", NA,         NA,
      NA,     "40 to 69", NA,
      "male", NA,         "white",
      "male", NA,         NA,
      "male", "40 to 69", NA,
      NA,     NA,         NA,
      NA,     NA,         "white",
      NA,     NA,         "white"),
    byrow = TRUE, ncol = 3L,
    dimnames = list(NULL, c("gender", "age_group", "race"))))
  levels(testData$gender) <- c("male", "female")
  levels(testData$age_group) <- c("40 to 69", "30 to 39", "20 to 29")
  levels(testData$race) <- c("white", "Hispanic")
  
  expect_equal(calcRisk(testData), rep_len(10, nrow(testData)))
})

