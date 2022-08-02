#' Feature selection step using the Relief algorithm
#'
#' Relief-based algorithms use nearest neighbors of randomly sampled
#' observations (without replacement) to derive feature weights/scores that
#' describe the relevance of each feature to the target variable. The feature
#' weights represent the differences between the normalized feature values from
#' each randomly sampled observation and a neighboring observation. If the
#' neighboring observation's class is the same as the sampled observation
#' (termed a 'hit') but the feature values are different, then this reduces the
#' score on the basis that widely varying feature values for the same class are
#' not desirable. Conversely, if a neighboring observation's class is different
#' from the sampled observation (termed a 'miss') and the feature values are
#' different, then this increases the score on the basis that observations of
#' different classes are widely separated by their feature values. The feature
#' weights / scores range from -1 (worst) to +1 (best).
#'
#' `step_select_relief` creates a *specification* of a recipe step that selects
#' a subset of predictors based on the scores of the relief algorithm. This step
#' requires the FSinR package to be installed. The top `top_p` scoring features,
#' or features whose scores occur in the top percentile `threshold` will be
#' retained as new predictors.
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param outcome A character string with the name of the response variable to
#'   use to evaluate information gain value against the predictors.
#' @param top_p An integer with the number of best scoring features to select.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best scoring features to select. Features with scores that are _larger_
#'   than the specified threshold will be retained, for example `threshold =
#'   0.9` will retain only predictors with scores in the top 90th percentile.
#'   Note that this overrides `top_p`.
#' @param neighbors An integer with the number of neighbors for find for each
#'   sampled instance. Default is 5.
#' @param sample_size An integer with the number of instances to sample. Default
#'   is 10.
#' @param exclude A character vector of predictor names that will be removed
#'   from the data. This will be set when `prep()` is used on the recipe and
#'   should not be set by the user.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and their information gain scores. This parameter is
#'   only produced after the recipe has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id 	A character string that is unique to this step to identify it.
#' @return A step_select_relief object.
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
#' data(cells, package = "modeldata")
#'
#' rec <-
#'  recipe(class ~ ., data = cells[, -1]) %>%
#'  step_select_relief(
#'    all_predictors(),
#'    outcome = "class",
#'    top_p = 10,
#'    threshold = 0.9
#'  )
#'
#' prepped <- prep(rec)
#'
#' new_data <- bake(prepped, new_data = NULL)
#' prepped
step_select_relief <- function(
    recipe, ...,
    outcome = NULL,
    role = NA,
    trained = FALSE,
    top_p = NA,
    threshold = NA,
    neighbors = 5,
    sample_size = 10,
    exclude = NULL,
    scores = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_relief")) {

  recipes::recipes_pkg_check("FSinR")

  if (neighbors <= 0)
    rlang::abort("`neighbors` must be greater than zero")

  if (sample_size <= 0)
    rlang::abort("'sample_size' must be greater than zero")

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_select_relief_new(
      terms = terms,
      trained = trained,
      outcome = outcome,
      role = role,
      top_p = top_p,
      threshold = threshold,
      neighbors = neighbors,
      sample_size = sample_size,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
step_select_relief_new <- function(terms, role, trained, outcome, top_p,
                                   threshold, neighbors, sample_size, exclude,
                                   scores, skip, id) {
  recipes::step(
    subclass = "select_relief",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    top_p = top_p,
    threshold = threshold,
    neighbors = neighbors,
    sample_size = sample_size,
    exclude = exclude,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @export
prep.step_select_relief <- function(x, training, info = NULL, ...) {
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- y_name[1]

  # check criteria
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  # feature selection
  if (length(x_names) > 0) {
    call_func <- rlang::call2(
      .fn = "relief",
      .ns = "FSelectorRcpp",
      x = rlang::quo(as.data.frame(training[, x_names])),
      y = rlang::quo(training[[y_name]]),
      neighboursCount = x$neighbors,
      sampleSize = x$sample_size
    )
    res <- rlang::eval_tidy(call_func)
    res <- as_tibble(res)
    res <- rlang::set_names(res, c("variable", "score"))
    res$score <- rlang::set_names(res$score, res$variable)
    res <- res[order(res$score, decreasing = TRUE), ]

    exclude <-
      select_percentile(res$score, x$top_p, x$threshold, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_relief_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    top_p = x$top_p,
    threshold = x$threshold,
    neighbors = x$neighbors,
    sample_size = x$sample_size,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_relief <- function(object, new_data, ...) {
  if (length(object$exclude > 0)) {
    new_data <- new_data[, !(colnames(new_data) %in% object$exclude)]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_relief <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Relief feature selection")

  if (recipes::is_trained(x)) {
    n <- length(x$exclude)
    cat(paste0(" (", n, " excluded)"))
  }
  cat("\n")

  invisible(x)
}

#' @rdname step_select_relief
#' @param x A `step_select_relief` object.
#' @export
tidy.step_select_relief <- function(x, ...) {
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
tunable.step_select_relief <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_select_relief",
    component_id = x$id
  )
}
