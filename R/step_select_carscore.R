#' Information gain feature selection step
#'
#' `step_select_carscore` creates a *specification* of a recipe step that
#' selects a subset of predictors as part of a regression model based on the
#' scores of the CAR score algorithm. This step requires the `care` package to be
#' installed. The top `top_p` scoring features, or features whose scores occur
#' in the top percentile `threshold` will be retained as new predictors.
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param lambda The correlation shrinkage intensity (range 0-1).
#' @param diagonal For diagonal = FALSE (the default) CAR scores are computed;
#'   otherwise with diagonal = TRUE marginal correlations.
#' @param outcome A character string with the name of the response variable.
#'   This must refer to a numeric feature for regression.
#' @param top_p An integer with the number of best scoring features to
#'   select.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best scoring features to select. Features with scores that are _larger_
#'   than the specified threshold will be retained, for example `threshold =
#'   0.9` will retain only predictors with scores in the top 90th percentile.
#'   Note that this overrides `top_p`.
#' @param exclude A character vector of predictor names that will be removed
#'  from the data. This will be set when `prep()` is used on the recipe and
#'  should not be set by the user.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and the absolute values of the calculated CAR
#'   scores. This parameter is only produced after the recipe has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id 	A character string that is unique to this step to identify it.
#' @return A step_select_carscore object.
#' @export
#' @keywords datagen
#' @concept preprocessing
#' @concept supervised_filter
#' @export
#' @details
#'
#' The recipe will stop if both `top_p` and `threshold` are left unspecified.
#'
#' @examples
#' library(recipes)
#'
#' data(car_prices, package = "modeldata")
#'
#' rec <-
#'  recipe(Price ~ ., data = car_prices) %>%
#'  step_select_carscore(all_predictors(), outcome = "Price", top_p = 5, threshold = 0.7)
#'
#' prepped <- prep(rec)
#'
#' new_data <- juice(prepped)
#' prepped
step_select_carscore <- function(
  recipe, ...,
  outcome = NULL,
  role = NA,
  trained = FALSE,
  top_p = NA,
  threshold = NA,
  lambda = NA,
  diagonal = FALSE,
  exclude = NULL,
  scores = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_carscore")) {

  recipes::recipes_pkg_check("care")

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_select_carscore_new(
      terms = terms,
      trained = trained,
      outcome = outcome,
      role = role,
      top_p = top_p,
      threshold = threshold,
      lambda = lambda,
      diagonal = diagonal,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
step_select_carscore_new <- function(terms, role, trained, outcome, top_p,
                                    threshold, lambda, diagonal, exclude, scores,
                                    skip, id) {
  recipes::step(
    subclass = "select_carscore",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    top_p = top_p,
    threshold = threshold,
    lambda = lambda,
    diagonal = diagonal,
    exclude = exclude,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @export
prep.step_select_carscore <- function(x, training, info = NULL, ...) {

  # extract response and predictor names
  x_names <- recipes::terms_select(terms = x$terms, info = info)
  y_name <- recipes::terms_select(x$outcome, info = info)
  y_name <- y_name[1]

  # check criteria
  recipes::check_type(training[, y_name], quant = TRUE)
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  # information gain
  if (length(x_names) > 0) {

    args <- list()

    if (!is.na(x$lambda))
      args$lambda <- x$lambda

    call <- rlang::call2(
      .fn = "carscore",
      .ns = "care",
      Xtrain = training[, x_names],
      Ytrain = training[, y_name],
      diagonal = x$diagonal,
      !!!args
    )

    res <- rlang::eval_tidy(call)

    res <- tibble(
      variable = names(res),
      score = abs(res)
    )

    exclude <-
      select_percentile(res$score, x$top_p, x$threshold, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_carscore_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    top_p = x$top_p,
    threshold = x$threshold,
    lambda = x$lambda,
    diagonal = x$diagonal,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_carscore <- function(object, new_data, ...) {
  if (length(object$exclude > 0)) {
    new_data <- new_data[, !(colnames(new_data) %in% object$exclude)]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_carscore <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Carscore feature selection")

  if(recipes::is_trained(x)) {
    n <- length(x$exclude)
    cat(paste0(" (", n, " excluded)"))
  }
  cat("\n")

  invisible(x)
}

#' @rdname step_select_carscore
#' @param x A `step_select_carscore` object.
#' @export
tidy.step_select_carscore <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = x$exclude)
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble(terms = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_select_carscore <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold"),
    call_info = list(
      list(pkg = "recipeselectors", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_select_carscore",
    component_id = x$id
  )
}
