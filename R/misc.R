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

select_percentile <- function(x, top_p, threshold, maximize) {
  # filter a named vector by the top_p features or using a percentile
  # threshold

  x <- x[!is.na(x)]

  if (!is.na(threshold)) {
    p_to_exceed <- stats::quantile(x, threshold)

    if (maximize) {
      removals <- x < p_to_exceed
    } else {
      removals <- x >= p_to_exceed
    }

    removals <- names(removals[removals])

  } else {
    if (maximize) {
      x <- sort(x, decreasing = TRUE)
    } else {
      x <- sort(x, decreasing = FALSE)
    }

    removals <- names(x[-seq_len(top_p)])
  }

  removals
}
