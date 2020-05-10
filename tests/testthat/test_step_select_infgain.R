library(testthat)
library(recipes)
library(tibble)
data("iris")

test_that("step_select_infgain, execution", {
  skip_if_not_installed("FSelectorRcpp")

  irisX <- iris[-5]
  y <- iris$Species

  ig_scores <- as_tibble(FSelectorRcpp::information_gain(x = irisX, y = y))
  ig_scores <- ig_scores[order(ig_scores$importance), ]
  ig_scores$importance <- rlang::set_names(ig_scores$importance, ig_scores$attributes)
  ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

  rec <- recipe(Species ~ ., data = iris)

  ig_rec <- rec %>%
    step_select_infgain(
      all_predictors(), outcome = "Species", type = "infogain", top_p = 2) %>%
    prep()

  ig_pred <- juice(ig_rec)
  expect_true(all(names(ig_pred)[1:2] %in% ig_scores$attributes[1:2]))
})


