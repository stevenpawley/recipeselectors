library(testthat)
library(recipes)
library(tibble)
library(parsnip)
library(modeldata)

data("cells")

test_that("step_select_linear, execution using top_p on binary case", {
  rec <- cells %>%
    select(-case) %>%
    recipe(class ~ .) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_select_linear(
      all_predictors(),
      outcome = "class",
      top_p = 2
    )

  prepped <- prep(rec)
  selected <- bake(prepped, new_data = NULL)

  expect_length(names(selected), 3)
})


test_that("step_select_linear, execution using threshold on binary case", {
  # test selection by retaining features with scores >= 50th percentile
  rec <- cells %>%
    select(-case) %>%
    recipe(class ~ .) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_select_linear(
      all_predictors(),
      outcome = "class",
      threshold = 0.99
    )

  prepped <- prep(rec)
  selected <- juice(prepped)

  expect_length(names(selected), 2)
})
