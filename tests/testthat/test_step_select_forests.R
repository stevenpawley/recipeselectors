library(testthat)
library(recipes)
library(tibble)
library(parsnip)

data("iris")

test_that("step_select_forests, execution using top_p", {
  skip_if_not_installed("ranger")

  rec <- iris %>%
    recipe(Species ~.) %>%
    step_select_forests(
      all_predictors(),
      outcome = "Species",
      engine = "ranger",
      top_p = 2
    )

  prepped <- prep(rec)
  tidy(rec, number = 1)
  selected <- juice(prepped)

  expect_length(names(selected), 3)
})


test_that("step_select_forests, execution using threshold", {
  skip_if_not_installed("ranger")

  irisX <- iris[-5]
  y <- iris$Species

  # test selection by retaining features with scores >= 50th percentile
  rec <- iris %>%
    recipe(Species ~.) %>%
    step_select_forests(
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
    step_select_forests(
      all_predictors(),
      outcome = "Species",
      threshold = 0.9
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 2)
})

