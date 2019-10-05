library(testthat)
library(recipes)
library(FSelectorRcpp)
library(tibble)

data("iris")
irisX <- iris[-5]
y <- iris$Species

ig_scores <- as_tibble(information_gain(x = irisX, y = y))
ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

rec <- recipe(Species ~ ., data = iris)

test_that("correct information gain scores", {
  skip_if_not_installed("FSelectorRcpp")

  ig_rec <- rec %>%
    step_infgain(
      all_predictors(), target = Species, type = "infogain", num_comp = 2) %>%
    prep()

  ig_pred <- juice(ig_rec)
  expect_true(all(names(ig_pred) %in% c(ig_scores$attributes[1:2], "Species")))

  expect_equal(ig_scores, ig_rec$steps[[1]]$scores)

})


