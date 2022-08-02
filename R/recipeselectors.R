#' recipeselectors: A collection of steps for feature selection to use with the
#' 'recipes' package
#'
#' \pkg{recipeselectors} provides a collection of additional step objects
#' related to feature selection to be used with the 'recipes' package.
#'
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' # load the example iris dataset
#' data(iris)
#'
#' # define a base model to use for feature importances
#' base_model <- rand_forest(mode = "classification") %>%
#'     set_engine("ranger", importance = "permutation")
#'
#' # create a preprocessing recipe
#' rec <- iris %>%
#'  recipe(Species ~ .) %>%
#'  step_select_vip(all_predictors(), model = base_model, top_p = 2,
#'                  outcome = "Species")
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
#' @name recipeselectors
NULL
