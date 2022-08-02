#' Pull feature importances from a parsnip fitted model
#'
#' `pull_importances` is a generic function to extract feature importance scores
#' or coefficients from a parsnip `model_fit` object and return them as a tibble
#' with a 'feature' and 'importance' column. This is designed to support the
#' `step_importance` recipe step.
#'
#' Most of the basic models within the parsnip package that support feature
#' importances are implemented (call `methods(pull_importances)` to list models
#' that are currently implemented). If need to pull the feature importance scores
#' from a model that is not currently supported in this package, then you can
#' add a class to the pull_importances generic function which returns a
#' two-column tibble:
#'
#' @param object A `model_fit` object.
#' @param scaled A logical indicating whether to rescale the importances between
#'   0 and 1. Default is TRUE.
#' @param ... A list of other parameters passed to the feature importance
#'   method.
#'
#' @return tibble
#' @export
#'
#' @examples
#' library(parsnip)
#'
#' # pull feature importances from a model_fit object
#' model <- boost_tree(mode = "classification") %>%
#'     set_engine("xgboost")
#' model_fit <- model %>% fit(Species ~., iris)
#' pull_importances(model_fit)
#'
#' # create a new pull_importances method
#' pull_importances._ranger <- function(object, scaled = FALSE, ...) {
#'     # create a call to the ranger::importance function avoiding having to use
#'     # ranger as a dependency
#'     call <- rlang::call2(.fn = "importance", .ns = "ranger", x = object$fit)
#'     scores <- rlang::eval_tidy(call)
#'
#'     # create a tibble with 'feature' and 'importance' columns
#'     scores <- tibble::tibble(
#'       feature = names(scores),
#'       importance = as.numeric(scores)
#'     )

#'     # optionally rescale the importance scores
#'     if (isTRUE(scaled))
#'       scores$importance <- rescale(scores$importance)
#'
#'     scores
#' }
pull_importances <- function(object, scaled = TRUE, ...) {
  UseMethod("pull_importances", object)
}


rescale <- function(x)
  (x - min(x)) / (max(x) - min(x)) * 100


#' @export
pull_importances.default <- function(object, scaled = TRUE, ...) {
  message(paste(
    "No method for pulling feature importances is defined for",
    class(object)[1]
  ))
}


#' @export
pull_importances._xgb.Booster <-
  function(object,
           scaled = TRUE,
           type = "Gain",
           ...) {

    call <- rlang::call2(
      .fn = "xgb.importance",
      .ns = "xgboost",
      model = object$fit
    )
    scores <- rlang::eval_tidy(call)
    scores <- tibble(feature = scores$Feature, importance = scores[[type]])

    if (scaled)
      scores$importance <- rescale(scores$importance)

    scores
  }

#' @export
pull_importances._C5.0 <- function(object, scaled = TRUE, ...) {
  others <- list(...)

  if (!length(others))
    others$metric = "usage"

  call <- rlang::call2(.fn = "C5imp", .ns = "C50", object = object$fit,!!!others)
  scores <- rlang::eval_tidy(call)

  scores <- tibble(feature = rownames(scores), importance = scores$Overall)

  if (scaled)
    scores$importance <- rescale(scores$importance)

  scores
}

#' @export
pull_importances._H2OMultinomialModel <-
  function(object, scaled = TRUE, ...) {
    call <- rlang::call2(.fn = "h2o.varimp", .ns = "h2o", object = object$fit)
    scores <- rlang::eval_tidy(call)

    scores <-
      tibble(feature = scores$variable, importance = scores$relative_importance)

    if (scaled)
      scores$importance <- rescale(scores$importance)

    scores
  }

#' @export
pull_importances._H2ORegressionModel <-
  function(object, scaled = TRUE, ...) {

    call <- rlang::call2(.fn = "h2o.varimp", .ns = "h2o", object = object$fit)
    scores <- rlang::eval_tidy(call)

    scores <-
      tibble(feature = scores$variable, importance = scores$relative_importance)

    if (scaled)
      scores$importance <- rescale(scores$importance)

    scores
  }

#' @export
pull_importances._ranger <- function(object, scaled = TRUE, ...) {
  call <- rlang::call2(.fn = "importance", .ns = "ranger", x = object$fit)
  scores <- rlang::eval_tidy(call)

  scores <- tibble(feature = names(scores), importance = as.numeric(scores))

  if (scaled)
    scores$importance <- rescale(scores$importance)

  scores
}

#' @export
pull_importances._cubist <- function(object, scaled = TRUE, ...) {
  scores <- object$fit$usage

  scores <- tibble(feature = scores$Variable, importance = scores$Model)

  if (scaled)
    scores$importance <- rescale(scores$importance)

  scores
}

#' @export
pull_importances._earth <- function(object, scaled = TRUE, ...) {
  call <- rlang::call2(.fn = "evimp", .ns = "earth", object = object$fit)
  scores <- rlang::eval_tidy(call)

  scores <- tibble(feature = rownames(scores), importance = scores[, "rss"])

  if (scaled)
    scores$importance <- rescale(scores$importance)

  scores
}

#' @export
pull_importances._lm <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           ...) {

    scores <- tibble(
      feature = names(stats::coefficients(object$fit)),
      importance = stats::coefficients(object$fit)
    )

    if (!intercept)
      scores <- scores[scores$feature != "(Intercept)",]

    if (scaled)
      scores$importance <- rescale(abs(scores$importance))

    scores
  }

#' @export
pull_importances._glm <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           ...) {

    scores <- tibble(feature = names(stats::coefficients(object$fit)),
                     importance = stats::coefficients(object$fit))

    if (!intercept)
      scores <- scores[scores$feature != "(Intercept)", ]

    if (scaled)
      scores$importance <- rescale(abs(scores$importance))

    scores
  }

#' @export
pull_importances._elnet <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           penalty = NULL,
           ...) {
    if (is.null(penalty))
      penalty <- object$spec$args$penalty

    if (is.null(penalty))
      rlang::abort(
        "model specification was not fitted using a `penalty` value. `penalty` should be supplied to the `pull_importances` method"
      )

    scores <- tibble(feature = rownames(stats::coef(object$fit, s = penalty)),
                     importance = stats::coef(object$fit, s = penalty)[, 1])

    if (!intercept)
      scores <- scores[scores$feature != "(Intercept)", ]

    if (scaled)
      scores$importance <- rescale(abs(scores$importance))

    scores
  }

#' @export
pull_importances._lognet <-
  function(object,
           scaled = FALSE,
           intercept = FALSE,
           penalty = NULL,
           ...) {
    if (!is.null(penalty)) {
      s <- penalty
    } else {
      s <- object$spec$args$penalty
    }

    if (is.null(s))
      rlang::abort(
        "model specification was not fitted using a `penalty` value. `penalty` should be supplied to the `pull_importances` method"
      )

    scores <- tibble(
      feature = rownames(stats::coef(object$fit, s = s)),
      importance = stats::coef(object$fit, s = s)[, 1]
    )

    if (!intercept)
      scores <- scores[scores$feature != "(Intercept)",]

    if (scaled)
      scores$importance <- rescale(abs(scores$importance))

    scores
  }

#' @export
pull_importances._randomForest <-
  function(object, scaled = TRUE, ...) {
    scores <- tibble(
      feature = rownames(object$fit$importance),
      importance = object$fit$importance
    )

    if (scaled)
      scores$importance <- rescale(scores$importance)

    scores
  }

#' @export
pull_importances._rpart <- function(object, scaled = TRUE, ...) {
  scores <- tibble(
    feature = names(object$fit$variable.importance),
    importance = object$fit$variable.importance
  )

  if (scaled)
    scores$importance <- rescale(scores$importance)

  scores
}

# stan?
# surv?
