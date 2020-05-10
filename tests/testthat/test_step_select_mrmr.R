library(testthat)
library(recipes)
library(tibble)
data("iris")

test_that("correct mrmr scores", {
  skip_if_not_installed("praznik")

  irisX <- iris[-5]
  y <- iris$Species

  res <- praznik::MRMR(X = irisX, Y = y, k = 4)

  mrmr_scores <- tibble(
    selection = res$selection,
    score = res$score,
    attribute = names(res$selection))

  rec <- recipe(Species ~ ., data = iris)

  mrmr_rec <- rec %>%
    step_mrmr(all_predictors(), target = Species, num_comp = 2) %>% prep()

  mrmr_pred <- juice(mrmr_rec)
  expect_true(all(names(mrmr_pred) %in% c(mrmr_scores$attribute[1:2], "Species")))

  expect_equal(mrmr_scores, mrmr_rec$steps[[1]]$scores)
})


