test_that("step_select_relief", {
  skip_if_not_installed("FSelectorRcpp")

  # FSelectorRcpp method
  set.seed(1234)
  raw <- FSelectorRcpp::relief(
    formula = Species ~ .,
    data = iris,
    neighboursCount = 5,
    sampleSize = 10
  )
  raw <- setNames(raw, c("variable", "score"))
  raw <- raw[order(raw$score, decreasing = TRUE), ]

  # test recipe
  rec <-
    recipe(Species ~ ., iris) %>%
    step_select_relief(all_predictors(), outcome = "Species", top_p = 2)

  set.seed(1234)
  prepped <- prep(rec)
  expect_equal(as.numeric(prepped$steps[[1]]$scores$score), raw$score)

  new_data <- bake(prepped, new_data = NULL)
  expect_equal(ncol(new_data), 3)
})
