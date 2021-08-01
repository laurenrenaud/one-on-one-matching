library(testthat)
library(tibble)

context("counting")

test.notes <- tibble(taker_name = c("Kevin", "Kim", "Nicole", "Sandra"),
                     with_name = c(rep("Sandra", 3), "Nicole"))

#' Number of unique conversations to be 3 -- 
#' Nicole and Sandra both left notes on their conversation
#' so we want to only count that once

test_that("Unique count as expected", {
  expect_equal(nrow(uniqueConversationPairs(test.notes)), 3)
})
