context("rsupp getAtRiskSubset")

testData <- data.frame(gender = c("male", "male", "male", "male", "female"),
                       age_group = c("10 to 17", "10 to 17", "18 to 19", "18 to 19", "20 to 29"),
                       race = c("white", "white", "white", "white", "black"))

test_that("fails with invalid inputs", {
  expect_error(getAtRiskSubset(testData, keyVars = "not-a-col"))
  expect_error(getAtRiskSubset(testData, divVar = "not-a-col"))
  expect_error(getAtRiskSubset(testData, divVar = "gender"))
  expect_error(getAtRiskSubset(testData, risk.f = "not-a-function"))
  expect_error(getAtRiskSubset(testData, risk.k = -1))
  expect_error(getAtRiskSubset(testData, risk.k = "not-a-number"))
  
  testData$temp <- LETTERS[nrow(testData)]
  expect_error(getAtRiskSubset(testData))
  testData$temp <- NULL
})

test_that("subsets correctly", {
  expect_equal(nrow(getAtRiskSubset(testData, risk.k = 3)), 5) # everyone
  expect_equal(nrow(getAtRiskSubset(testData, risk.k = 2)), 1) # female
  expect_equal(nrow(getAtRiskSubset(testData, risk.k = 1)), 0) # no-one
})

