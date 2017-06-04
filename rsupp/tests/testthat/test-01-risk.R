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

