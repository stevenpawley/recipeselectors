library(testthat)
library(recipes)
library(tibble)
library(modeldata)

data("lending_club")

test_that("step_select_boruta, execution", {
  skip_if_not_installed("Boruta")

  # Boruta model results
  set.seed(1234)
  boruta_mod <- Boruta::Boruta(
    x = lending_club[, -23],
    y = lending_club$Class
  )
  excluded <- names(
    boruta_mod$finalDecision[boruta_mod$finalDecision == "Rejected"]
  )

  # step_select_boruta results
  rec <- recipe(Class ~ ., data = lending_club) %>%
    step_select_boruta(all_predictors(), outcome = "Class")
  set.seed(1234)
  prepped <- rec %>% prep()

  # check
  expect_equal(excluded, prepped$steps[[1]]$exclude)
  expect_equal(boruta_mod$ImpHistory, prepped$steps[[1]]$res$ImpHistory)
})


test_that("step_select_boruta, options", {
  skip_if_not_installed("Boruta")

  # Boruta model results
  set.seed(1234)
  boruta_mod <- Boruta::Boruta(
    x = lending_club[, -23],
    y = lending_club$Class,
    getImp = Boruta::getImpRfGini
  )
  excluded <- names(
    boruta_mod$finalDecision[boruta_mod$finalDecision == "Rejected"]
  )

  # step_select_boruta results
  rec <- recipe(Class ~ ., data = lending_club) %>%
    step_select_boruta(all_predictors(), outcome = "Class",
                       options = list(getImp = Boruta::getImpRfGini))
  set.seed(1234)
  prepped <- rec %>% prep()

  # check
  expect_equal(excluded, prepped$steps[[1]]$exclude)
  expect_equal(boruta_mod$ImpHistory, prepped$steps[[1]]$res$ImpHistory)
})

