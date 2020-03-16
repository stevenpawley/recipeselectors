library(testthat)
library(recipes)
library(tibble)
library(parsnip)
library(ranger)
data("iris")

test_that("feature importance selection using num_comp", {
  skip_if_not_installed("ranger")

  irisX <- iris[-5]
  y <- iris$Species

  base_model <- rand_forest(mode = "classification") %>%
    set_engine("ranger", importance = "permutation")

  rec <- iris %>%
    recipe(Species ~.) %>%
    step_importance(
      all_predictors(),
      target = Species,
      model = base_model,
      num_comp = 2
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 3)
})


test_that("feature importance selection using threshold", {
  skip_if_not_installed("ranger")

  irisX <- iris[-5]
  y <- iris$Species

  base_model <- rand_forest(mode = "classification") %>%
    set_engine("ranger", importance = "permutation")

  # test selection by retaining features with scores >= 50th percentile
  rec <- iris %>%
    recipe(Species ~.) %>%
    step_importance(
      all_predictors(),
      target = Species,
      model = base_model,
      threshold = 0.5
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 3)

  # test selection by retaining features with scores in 90th percentile
  rec <- iris %>%
    recipe(Species ~.) %>%
    step_importance(
      all_predictors(),
      target = Species,
      model = base_model,
      threshold = 0.9
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 2)
})

