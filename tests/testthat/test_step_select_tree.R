library(testthat)
library(recipes)
library(tibble)
library(parsnip)
data("iris")

test_that("step_select_tree, execution using top_p", {
  skip_if_not_installed("rpart")

  irisX <- iris[-5]
  y <- iris$Species

  rec <- iris %>%
    recipe(Species ~.) %>%
    step_select_tree(
      all_predictors(),
      outcome = "Species",
      engine = "rpart",
      top_p = 2
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 3)
})


test_that("step_select_tree, execution using threshold", {
  skip_if_not_installed("rpart")

  irisX <- iris[-5]
  y <- iris$Species

  # test selection by retaining features with scores >= 50th percentile
  rec <- iris %>%
    recipe(Species ~.) %>%
    step_select_tree(
      all_predictors(),
      outcome = "Species",
      threshold = 0.5
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 3)

  # test selection by retaining features with scores in 90th percentile
  rec <- iris %>%
    recipe(Species ~.) %>%
    step_select_tree(
      all_predictors(),
      outcome = "Species",
      threshold = 0.9
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 2)
})
