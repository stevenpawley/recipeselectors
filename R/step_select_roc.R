#' Filter Numeric Predictors using ROC Curve
#'
#' `step_select_roc` creates a *specification* of a recipe step that will
#'  filter predictors using their relationship with the outcome as measured
#'  using a Receiver Operating Characteristic curve.
#'
#' @param ... One or more selector functions to choose which predictors are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param class A single character string that specifies a single categorical
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


check_zero_one <- function(x) {
  if (is.na(x)) {
    return(x)
  } else {
    if (is.numeric(x)) {
      if (x >= 1 | x <= 0) {
        rlang::abort("`threshold` should be on (0, 1).")
      }
    } else {
      rlang::abort("`threshold` should be numeric.")
    }
  }
  return(x)
}

check_top_p <- function(x, n) {
  if (is.na(x)) {
    return(x)
  } else {
    if (is.numeric(x)) {
      if (!is.integer(x)) {
        x <- as.integer(x)
      }
      if (x >= n | x <= 0) {
        msg <- paste0("`top_p` should be on (0, ", n, ").")
        rlang::warn(msg)
        x <- min(n - 1, x)
      }
    } else {
      rlang::abort("`top_p` should be numeric.")
    }
  }
  x
}

check_criteria <- function(top_p, threshold, cl) {
  if (is.na(top_p) & is.na(threshold)) {
    msg <- paste0(
      "For `",
      cl[[1]],
      "`, `top_p` and `threshold` cannot both be missing."
    )
    rlang::abort(msg)
  }
  invisible(NULL)
}

roc_calc <- function(x, y) {
  withr::with_message_sink(
    tempfile(),
    {
      if (length(levels(y)) == 2) {
        res <- try(pROC::roc(y, x, direction = "auto"), silent = TRUE)
      } else {
        res <- try(pROC::multiclass.roc(y, x, direction = "auto"), silent = TRUE)
      }
    }
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
    scores <- rev(sort(scores))

    # This part should be made into a reusable function
    if (!is.na(x$top_p)) {
      top_p_lgl <- seq_along(scores) <= x$top_p
      top_p_lgl[is.na(top_p_lgl)] <- FALSE
    } else {
      top_p_lgl <- rep(TRUE, length(scores))
    }

    if (!is.na(x$threshold)) {
      threshold_lgl <- scores >= x$threshold
      threshold_lgl[is.na(threshold_lgl)] <- FALSE
    } else {
      threshold_lgl <- rep(FALSE, length(scores))
    }
    keep_lgl <- top_p_lgl | threshold_lgl
    exclude_chr <- names(scores)[!keep_lgl]
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
  cat("ROC curve feature selection\n")
  invisible(x)
}

#' @rdname step_select_roc
#' @param x A `step_select_roc` object.
#' @export
tidy.step_select_roc <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = x$exclude)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = na_chr)
  }
  res$id <- x$id
  res
}


#' @export
top_p <- function(range = c(1L, 4L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(top_p = "# Selected Predictors"),
    finalize = dials::get_p
  )
}


#' @export
#' @export tunable.step_select_roc
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

