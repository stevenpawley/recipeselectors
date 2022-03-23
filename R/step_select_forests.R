#' Feature selection step using a random forest feature importance scores
#'
#' `step_select_forests` creates a *specification* of a recipe step that selects
#' a subset of predictors based on the ranking of variable importance using
#' a `parsnip::rand_forest` supported model.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used.
#' @param outcome A character string with the name of the response variable to
#'   use to calculate the feature importance scores.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param engine A supported rand_forest engine that is supported by parsnip.
#'   The default is "ranger".
#' @param options A named list of options to pass to the rand_forest engine. For
#'   example, if `engine = 'ranger'` (the default) then options could be
#'   `list(permutation = 'importance`) because a feature importance method needs
#'   to be specified for this engine. This is the default.
#' @param top_p An integer with the number of best scoring features to
#'   select.
#' @param mtry An integer for the number of predictors that will be randomly
#'   sampled at each split when creating the tree models.
#' @param trees An integer for the number of trees contained in the ensemble.
#' @param min_n An integer for the minimum number of data points in a node that
#'   are required for the node to be split further.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best scoring features to select. Features with scores that are _larger_
#'   than the specified threshold will be retained, for example `threshold =
#'   0.9` will retain only predictors with scores in the top 90th percentile.
#'   Note that this overrides `top_p`.
#' @param exclude A character vector of predictor names that will be removed
#'  from the data. This will be set when `prep()` is used on the recipe and
#'  should not be set by the user.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and their feature importance scores. This parameter
#'   is only produced after the recipe has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return a `step_select_forests` object.
#' @export
#' @examples
#' library(recipes)
#' library(parsnip)
#'
#' # load the example iris dataset
#' data(cells, package = "modeldata")
#'
#' # create a preprocessing recipe
#' rec <-
#'  recipe(class ~ ., data = cells[, -1]) %>%
#'  step_select_forests(all_predictors(), outcome = "class", top_p = 10,
#'                      threshold = 0.9)
#'
#' prepped <- prep(rec)
#'
#' preproc_data <- juice(prepped)
#' prepped
step_select_forests <- function(
    recipe,
    ...,
    outcome = NULL,
    role = "predictor",
    trained = FALSE,
    engine = "ranger",
    options = list(importance = "permutation"),
    mtry = NULL,
    trees = NULL,
    min_n = NULL,
    top_p = NA,
    threshold = NA,
    exclude = NULL,
    scores = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_forests")) {

  engines <- parsnip::show_engines("rand_forest")$engine

  if (!engine %in% parsnip::show_engines("rand_forest")$engine)
    rlang::abort(
      paste("Engine argument should be one of", paste(engines, collapse = ", "))
    )

  recipes::add_step(
    recipe,
    step_select_forests_new(
      terms = recipes::ellipse_check(...),
      trained = trained,
      outcome = outcome,
      role = role,
      engine = engine,
      options = options,
      mtry = mtry,
      trees = trees,
      min_n = min_n,
      top_p = top_p,
      threshold = threshold,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_forests_new <- function(terms, role, trained, outcome, engine,
                                    options, top_p, mtry, trees, min_n,
                                    threshold, exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_forests",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    engine = engine,
    options = options,
    mtry = mtry,
    trees = trees,
    min_n = min_n,
    top_p = top_p,
    threshold = threshold,
    exclude = exclude,
    scores = scores,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_select_forests <- function(x, training, info = NULL, ...) {

  # translate the terms arguments
  x_names <- recipes::terms_select(terms = x$terms, info = info)
  y_name <- recipes::terms_select(x$outcome, info = info)
  y_name <- y_name[1]

  # check criteria
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  if (length(x_names) > 0) {
    # fit initial model
    X <- training[, x_names]
    y <- training[[y_name]]

    model_mode <- ifelse(inherits(y, "numeric"), "regression", "classification")

    model_args <- list(
      trees = x$trees,
      mtry = x$mtry,
      min_n = x$min_n
    )

    model_spec <-
      parsnip::make_call("rand_forest", args = model_args, ns = "parsnip")

    model_spec <-
      rlang::eval_tidy(model_spec) %>%
      parsnip::set_mode(model_mode) %>%
      parsnip::set_engine(x$engine, !!!x$options)

    initial_model <- parsnip::fit_xy(model_spec, X, y)
    res <- pull_importances(initial_model)
    names(res) <- c("variable", "score")
    res$score <- rlang::set_names(res$score, res$variable)

    exclude <-
      select_percentile(res$score, x$top_p, x$threshold, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_forests_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    engine = x$engine,
    options = x$options,
    mtry = x$mtry,
    trees = x$trees,
    min_n = x$min_n,
    top_p = x$top_p,
    threshold = x$threshold,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_forests <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }

  as_tibble(new_data)
}

#' @export
print.step_select_forests <- function(x, width = max(20, options()$width - 30),
                                      ...) {
  cat("Variable importance feature selection")

  if (recipes::is_trained(x)) {
    n <- length(x$exclude)
    cat(paste0(" (", n, " excluded)"))
  }
  cat("\n")

  invisible(x)
}

#' @rdname step_select_forests
#' @param x A `step_select_forests` object.
#' @export
tidy.step_select_forests <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = x$exclude)

  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_select_forests <- function(x, ...) {
  tibble(
    name = c("top_p", "threshold", "mtry", "trees", "min_n"),
    call_info = list(
      list(pkg = "recipeselectors", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "dials", fun = "mtry", range = c(1L, dials::unknown())),
      list(pkg = "dials", fun = "trees", range = c(1L, 2000L)),
      list(pkg = "dials", fun = "min_n", range = c(2L, 40L))
    ),
    source = "recipe",
    component = "step_select_forests",
    component_id = x$id
  )
}
