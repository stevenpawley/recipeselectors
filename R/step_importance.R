#' Feature selection step using a model's feature importance scores or
#' coefficients
#'
#' `step_importance` creates a *specification* of a recipe step that selects a
#' subset of predictors based on the ranking of variable importance provided by
#' a `parsnip` model specification and the `model` parameter
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used.
#' @param target name of response variable to use to evaluate the importance of
#'   the predictors against.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param model A `model_spec` object from `parsnip`.
#' @param num_comp numeric, the number of best scoring features to select.
#' @param threshold numeric, percentile of best features to select. Note that
#' this overrides num_comp.
#' @param to_retain character, names of features to retain.
#' @param scores tibble, tibble of feature importance scores.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return a `step_importance` object.
#' @importFrom recipes recipes_pkg_check add_step
#' @export
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
#'  step_importance(all_predictors(), model = base_model, num_comp = 2,
#'                  target = Species, id = "importance_filter")
#'
#' prepped <- prep(rec)
step_importance <- function(
  recipe, ...,
  target = NULL,
  role = "predictor",
  trained = FALSE,
  model = NULL,
  num_comp = NULL,
  threshold = NULL,
  to_retain = NULL,
  scores = NULL,
  skip = FALSE,
  id = rand_id("importance1")) {

  if (missing(model))
    rlang::abort("Model argument should be a `parsnip` model specification")

  add_step(
    recipe,
    step_importance_new(
      terms = ellipse_check(...),
      trained = trained,
      target = enquos(target),
      role = role,
      model = model,
      num_comp = num_comp,
      threshold = threshold,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_importance_new <- function(terms, role, trained, target, model, num_comp,
                                threshold, to_retain, scores, skip, id) {
  step(
    subclass = "importance",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    model = model,
    num_comp = num_comp,
    threshold = threshold,
    to_retain = to_retain,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @importFrom tibble tibble
#' @importFrom stats quantile
#' @importFrom recipes terms_select check_type
#' @importFrom parsnip set_engine set_mode fit_xy
#' @importFrom rlang eval_tidy enquos
#' @export
prep.step_importance <- function(x, training, info = NULL, ...) {

  # first translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- terms_select(x$target, info = info)

  # fit initial model and get feature importances
  X <- training[, col_names]
  y <- training[[target_name]]

  initial_model <- x$model %>% fit_xy(X, y)
  feature_ranking <- pull_importances(initial_model)
  feature_ranking <- feature_ranking[order(-feature_ranking$importance), ]

  # select k best features
  if (!is.null(x$threshold)) {
    score_to_exceed <- quantile(feature_ranking$importance, x$threshold)
    x$num_comp <- max(which(feature_ranking$importance >= score_to_exceed))
  }

  if (is.null(x$num_comp) & is.null(x$threshold))
    x$num_comp <- length(col_names)

  to_retain <- c(feature_ranking[1:x$num_comp, ][["feature"]], target_name)

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE
  step_importance_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    model = x$model,
    num_comp = x$num_comp,
    threshold = x$threshold,
    to_retain = to_retain,
    scores = feature_ranking,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom tibble as_tibble
#' @export
bake.step_importance <- function(object, new_data, ...) {
  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]
  as_tibble(new_data)
}


#' @importFrom recipes format_ch_vec
#' @export
print.step_importance <- function(x, width = max(20, options()$width - 40), ...) {
  if (x$trained) {
    if (x$num_comp == 1) {
      cat("No features were extracted.\n")
    } else {
      cat("Feature importance (", x$scores, ") extraction with ", sep = "")
      cat(format_ch_vec(
        colnames(x$scores), width = width))
    }
  }
  if (x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


#' @export
tunable.step_importance <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 10)),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_importance",
    component_id = x$id
  )
}
