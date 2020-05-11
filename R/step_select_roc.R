#' Filter Numeric Predictors using ROC Curve
#'
#' `step_select_roc` creates a *specification* of a recipe step that will
#'  filter predictors using their relationship with the outcome as measured
#'  using a Receiver Operating Characteristic curve.
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which predictors are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param outcome A single character string that specifies a single categorical
#'  variable to be used as the class.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that resulting distances
#'  will be used as predictors in a model.
#' @param threshold A numeric value, in AUC units, where predictors with ROC
#'  AUC values _larger_ than the threshold will be retained. A value of `NA`
#'  implies that this criterion will be ignored.
#' @param top_p An integer that will be used to select the predictors with the
#'  largest ROC AUC values. A value of `NA` implies that this criterion will be
#'  ignored.
#' @param exclude A character vector of predictor names that will be removed
#'  from the data. This will be set when `prep()` is used on the recipe and
#'  should not be set by the user.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id 	A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with a `terms` column for which predictors were
#' removed.
#' @keywords datagen
#' @concept preprocessing
#' @concept supervised_filter
#' @export
#' @details
#'
#' The recipe will stop if both `top_p` and `threshold` are left unspecified.
#'
#' The ROC AUC will be set to be 1 - AUC if the value is less than 0.50.
#' @examples
#' data(cells, package = "modeldata")
#'
#' rec <-
#'   recipe(class ~ ., data = cells[, -1]) %>%
#'   step_select_roc(all_predictors(), outcome = "class", top_p = 10, threshold = 0.9) %>%
#'   prep()
#'
#' rec %>% juice(all_predictors()) %>% names()
#'
#' # Use ROC values to select but always keep at least one:
#' rec <-
#'   recipe(class ~ ., data = cells[, -1]) %>%
#'   step_select_roc(all_predictors(), outcome = "class", top_p = 1, threshold = 0.99) %>%
#'   prep()
#'
#' rec %>% juice(all_predictors()) %>% names()
#'
#' # in case of missing data...
step_select_roc <- function(recipe,
                           ...,
                           outcome,
                           role = "predictor",
                           trained = FALSE,
                           threshold = NA,
                           top_p = NA,
                           exclude = NULL,
                           skip = FALSE,
                           id = rand_id("select_roc")) {
  add_step(
    recipe,
    step_select_roc_new(
      terms = ellipse_check(...),
      outcome = outcome,
      role = role,
      trained = trained,
      threshold = threshold,
      top_p = top_p,
      exclude = exclude,
      skip = skip,
      id = id
    )
  )
}

step_select_roc_new <-
  function(terms, outcome, role, trained, threshold, top_p, exclude, skip, id) {
    step(
      subclass = "select_roc",
      terms = terms,
      outcome = outcome,
      role = role,
      trained = trained,
      threshold = threshold,
      top_p = top_p,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }

roc_calc <- function(x, y) {
  suppressMessages(
    suppressWarnings(
      {
        if (length(levels(y)) == 2) {
          res <- try(pROC::roc(y, x, direction = "auto"), silent = TRUE)
        } else {
          res <- try(pROC::multiclass.roc(y, x, direction = "auto"), silent = TRUE)
        }
      }
    )
  )

  if (inherits(res, "try-error")) {
    res <- NA_real_
  } else {
    res <- unname(pROC::auc(res))
  }
  res
}


#' @export
prep.step_select_roc <- function(x, training, info = NULL, ...) {
  y_name <- terms_select(x$outcome, info = info)
  y_name <- x$outcome[1]
  check_type(training[, y_name], quant = FALSE)
  x_names <- terms_select(x$terms, info = info, empty_fun = I)

  if(length(x_names) > 0) {

    check_type(training[, x_names])

    # check criteria
    check_criteria(x$top_p, x$threshold, match.call())
    check_zero_one(x$threshold)
    x$top_p <- check_top_p(x$top_p, length(x_names))

    # filter
    scores <- purrr::map_dbl(training[, x_names], ~ roc_calc(.x, training[[y_name]]))
    exclude_chr <- dual_filter(scores, x$top_p, x$threshold, maximize = TRUE)
  } else {
    exclude_chr <- character()
  }

  step_select_roc_new(
    terms = x$terms,
    outcome = x$outcome,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    top_p = x$top_p,
    exclude = exclude_chr,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_select_roc <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data %>% dplyr::select(-dplyr::one_of(object$exclude))
  }
  new_data
}

#' @export
print.step_select_roc <- function(x, width = max(20, options()$width - 30), ...) {
  cat("ROC curve feature selection")

  if(recipes::is_trained(x)) {
    n <- length(x$exclude)
    cat(paste0(" (", n, " excluded)"))
  }
  cat("\n")

  invisible(x)
}

#' @rdname step_select_roc
#' @param x A `step_select_roc` object.
#' @export
tidy.step_select_roc <- function(x, ...) {
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
#' @rdname tunable
tunable.step_select_roc <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold"),
    call_info = list(
      list(pkg = "recipeselectors", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_select_roc",
    component_id = x$id
  )
}
