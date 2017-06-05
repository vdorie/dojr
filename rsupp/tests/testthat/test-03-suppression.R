context("rsupp localSuppression")

testData <- data.frame(gender = c("male", "male", "male", "male", "female"),
                       age_group = c("10 to 17", "10 to 17", "18 to 19", "18 to 19", "20 to 29"),
                       race = c("white", "white", "white", "white", "black"))

test_that("fails with invalid data inputs", {
  expect_error(localSuppression(testData, keyVars = "not-a-col"))
  expect_error(localSuppression(testData, divVar = "not-a-col"))
  expect_error(localSuppression(testData, divVar = "gender"))
  expect_error(localSuppression(testData, risk.f = "not-a-function"))
  expect_error(localSuppression(testData, risk.k = -1))
  expect_error(localSuppression(testData, risk.k = "not-a-number"))
  expect_error(localSuppression(testData, strataVars = "not-a-col"))
  expect_error(localSuppression(testData, keyVars = c("gender", "age_group", "race"), strataVars = "gender"))
  
  testData$temp <- LETTERS[nrow(testData)]
  expect_error(localSuppression(testData))
  testData$temp <- NULL
})

test_that("fails with invalid mcmc inputs", {
  expect_error(localSuppression(testData, keyVars.w = c("not-a-col")))
  expect_error(localSuppression(testData, keyVars.w = -1))
  expect_error(localSuppression(testData, keyVars.w = c(not_a_col = 1)))
  expect_error(localSuppression(testData, par = rsupp.par(alpha = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(alpha = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(gamma = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(gamma = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(n.burn = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(n.burn = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(n.samp = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(n.samp = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(n.chain = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(n.chain = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(rowSwap.prob = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(rowSwap.prob = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(colSwap.prob = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(colSwap.prob = "not-a-number")))
  expect_error(localSuppression(testData, par = rsupp.par(na.prob = -1)))
  expect_error(localSuppression(testData, par = rsupp.par(na.prob = "not-a-number")))
})

test_that("returns trivial solutions", {
  ## no solution possible
  res <- localSuppression(testData, risk.k = 2)
  expect_true(all(is.na(res$x[,c("gender", "age_group", "race")])))
  
  ## already above threshold
  res <- localSuppression(testData, risk.k = 1)
  expect_equal(res$x[,c("gender", "age_group", "race")], testData)
})

test_that("random init returns non-trivial solution", {
  testData <- data.frame(gender = c("male", "male", "male", "male", "male"),
                         age_group = c("10 to 17", "10 to 17", "18 to 19", "18 to 19", "18 to 19"),
                         race = c("white", "white", "white", "white", "black"))
  ## problem should be solvable with only a single NA
  res <- localSuppression(testData, risk.k = 2, par = rsupp.par(n.burn = 0, n.samp = 0))
  expect_true(sum(is.na(res$x[,c("gender", "age_group", "race")])) == 2L)
  expect_true(all(res$x[,"risk"] >= 2))
})
