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
p_threshold <- function(range = c(-10, -0.001), trans = scales::log10_trans()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = -2,
    label = c(p_threshold = "Threshold"),
    finalize = NULL
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


dual_filter <- function(x, top_p, threshold, maximize) {
  na_x <- x[ is.na(x)]
  x <- x[!is.na(x)]
  x <- sort(x)
  if (maximize) {
    x <- rev(x)
  }
  p <- length(x)

  if (!is.na(top_p)) {
    top_p_lgl <- seq_along(x) <= top_p
  } else {
    top_p_lgl <- rep(FALSE, p)
  }

  if (!is.na(threshold)) {
    if (maximize) {
      threshold_lgl <- x >= threshold
    } else {
      threshold_lgl <- x <= threshold
    }
  } else {
    threshold_lgl <- rep(FALSE, p)
  }
  keep_lgl <- top_p_lgl | threshold_lgl
  c(names(x)[!keep_lgl], names(na_x))
}

