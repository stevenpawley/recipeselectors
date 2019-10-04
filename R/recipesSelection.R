#' recipesSelection: A collection of steps for feature selection to use with the 'recipes' package
#'
#' \pkg{recipesSelection} provides a collection of additional step objects related to feature
#' selection to be used with the 'recipes' package.
#'
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(dplyr)
#'
#' # load the example iris dataset
#' data(iris)
#'
#' # create a preprocessing recipe
#' fimp_model <- rand_forest(mode = "classification", trees = 100) %>%
#'     set_engine("ranger", importance = "permutation")
#'
#' rec <- iris %>%
#'  recipe(Species ~ .) %>%
#'  step_ranger_fimp(all_predictors(), threshold = 0.5, target = Species,
#'                   model = fimp_model, id = "importance_filter")
#'
#' prepped <- prep(rec)
#'
#' # create a model specification
#' clf <- decision_tree(mode = "classification") %>%
#'     set_engine("rpart")
#'
#' clf_fitted <- clf %>%
#'     fit(Species ~ ., juice(prepped))
#'
#' @author Steven Pawley, \email{dr.stevenpawley@@gmail.com}

#' @docType package
#' @name recipesSelection
NULL
