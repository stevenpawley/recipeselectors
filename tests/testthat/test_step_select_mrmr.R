library(testthat)
library(recipes)
library(tibble)
data("iris")

test_that("step_select_mrmr, execution", {
  skip_if_not_installed("praznik")

  irisX <- iris[-5]
  y <- iris$Species

  res <- praznik::MRMR(X = irisX, Y = y, k = 4)

  mrmr_scores <- tibble(
    variable = names(res$score),
    scores = res$score
  )

  rec <- recipe(Species ~ ., data = iris)

  mrmr_rec <- rec %>%
    step_select_mrmr(all_predictors(), outcome = "Species", top_p = 2) %>%
    prep()

  mrmr_pred <- juice(mrmr_rec)
  expect_true(all(names(mrmr_pred)[1:2] %in% mrmr_scores$variable[1:2]))

  expect_equal(mrmr_scores$scores, mrmr_rec$steps[[1]]$scores$score)
})


