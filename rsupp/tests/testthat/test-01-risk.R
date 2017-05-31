context("rsupp calcRisk")

testData <- data.frame(gender = c("male", "male", "male", "male", "female"),
                       age_group = c("10 to 17", "10 to 17", "18 to 19", "18 to 19", "20 to 29"),
                       race = c("white", "white", "white", "white", "black"),
                       offense_level = c("misdemeanor", "misdemeanor", "misdemeanor", "felony", "misdemeanor"))

test_that("fails with invalid inputs", {
  expect_error(calcRisk(testData, keyVars = "not-a-col"))
  expect_error(calcRisk(testData, div = "not-a-col"))
  expect_error(calcRisk(testData, div = "gender"))
  expect_error(calcRisk(testData, risk.f = "not-a-function"))
  
  testData$temp <- LETTERS[nrow(testData)]
  expect_error(getAtRiskSubset(testData))
  testData$temp <- NULL
})

test_that("computes risk correctly", {
  ## k-anon
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race")),
               c(2, 2, 2, 2, 1))
  ## l-diversity
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), div = "offense_level",
                        risk.f = function(x) as.double(sum(x > 0))),
               c(1, 1, 2, 2, 1))
  ## soft ordering
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), div = "offense_level",
                        risk.f = function(x) as.double(x["misdemeanor"] > 0L)),
               c(1, 1, 1, 1, 1))
  ## percent
  expect_equal(calcRisk(testData, keyVars = c("gender", "age_group", "race"), div = "offense_level",
                        risk.f = function(x) x["misdemeanor"] / sum(x)),
               c(1, 1, 0.5, 0.5, 1))
})

