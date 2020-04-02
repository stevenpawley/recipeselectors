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
pull_importances <- function(object, ...) {
  UseMethod("pull_importances", object)
}


rescale <- function(x)
  (x-min(x))/(max(x) - min(x)) * 100


#' @export
pull_importances.default <- function(object, scaled = FALSE, ...) {
  message(paste(
    "No method for pulling feature importances is defined for",
    class(object)[1]
  ))
}


#' @export
pull_importances._xgb.Booster <- function(object, scaled = FALSE, ...) {

  call <- rlang::call2(
    .fn = "xgb.importance",
    .ns = "xgboost",
    model = object$fit
  )
  scores <- rlang::eval_tidy(call)

  scores <- tibble::tibble(
    feature = scores$Feature,
    importance = scores$Gain
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


#' @export
pull_importances._C5.0 <- function(object, scaled = FALSE, ...) {
  others <- list(...)

  if (!length(others))
    others$metric = "usage"

  call <- rlang::call2(
    .fn = "C5imp",
    .ns = "C50",
    object = object$fit,
    !!!others)

  scores <- rlang::eval_tidy(call)

  scores <- tibble::tibble(
    feature = rownames(scores),
    importance = scores$Overall
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


#' @export
pull_importances._H2OMultinomialModel <- function(object, scaled = FALSE, ...) {

  call <- rlang::call2(
    .fn = "h2o.varimp",
    .ns = "h2o",
    object = object$fit
  )

  scores <- rlang::eval_tidy(call)

  scores <- tibble::tibble(
    feature = scores$variable,
    importance = scores$relative_importance
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}

#' @export
pull_importances._H2ORegressionModel <- function(object, scaled = FALSE, ...) {

  call <- rlang::call2(
    .fn = "h2o.varimp",
    .ns = "h2o",
    object = object$fit
  )

  scores <- rlang::eval_tidy(call)

  scores <- tibble::tibble(
    feature = scores$variable,
    importance = scores$relative_importance
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


#' @export
pull_importances._ranger <- function(object, scaled = FALSE, ...) {

  call <- rlang::call2(
    .fn = "importance",
    .ns = "ranger",
    x = object$fit
  )

  scores <- rlang::eval_tidy(call)

  scores <- tibble::tibble(
    feature = names(scores),
    importance = as.numeric(scores)
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


#' @export
pull_importances._cubist <- function(object, scaled = FALSE, ...) {
  scores <- object$fit$usage

  scores <- tibble::tibble(
    feature = scores$Variable,
    importance = scores$Model
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


#' @export
pull_importances._earth <- function(object, ...) {
  others <- list(...)

  call <- rlang::call2(
    .fn = "evimp",
    .ns = "earth",
    object = object$fit
  )

  rlang::call_modify(call, !!!others)

  scores <- rlang::eval_tidy(call)

  scores <- tibble::tibble(
    feature = rownames(scores),
    importance = scores[, "rss"]
  )

  scores
}


#' @export
pull_importances._lm <- function(object, intercept = FALSE, ...) {
  others <- list(...)

  scores <- tibble::tibble(
    feature = names(coefficients(object$fit)),
    importance = coefficients(object$fit)
  )

  if (isFALSE(intercept))
    scores <- scores[scores$feature != "(Intercept)", ]

  scores
}


#' @export
pull_importances._glm <- function(object, intercept = FALSE, ...) {
  others <- list(...)

  scores <- tibble::tibble(
    feature = names(coefficients(object$fit)),
    importance = coefficients(object$fit)
  )

  if (isFALSE(intercept))
    scores <- scores[scores$feature != "(Intercept)", ]

  scores
}


#' @export
pull_importances._elnet <- function(object, intercept = FALSE, penalty = NULL, ...) {

  if (!is.null(penalty)) {
    s <- penalty
  } else {
    s <- object$spec$args$penalty
  }

  if (is.null(s))
    rlang::abort("model specification was not fitted using a `penalty` value. `penalty` should be supplied to the `pull_importances` method")

  scores <- tibble::tibble(
    feature = rownames(coef(object$fit, s = s)),
    importance = coef(object$fit, s = s)[, 1]
  )

  if (isFALSE(intercept))
    scores <- scores[scores$feature != "(Intercept)", ]

  scores
}


#' @export
pull_importances._lognet <- function(object, intercept = FALSE, penalty = NULL, ...) {

  if (!is.null(penalty)) {
    s <- penalty
  } else {
    s <- object$spec$args$penalty
  }

  if (is.null(s))
    rlang::abort("model specification was not fitted using a `penalty` value. `penalty` should be supplied to the `pull_importances` method")

  scores <- tibble::tibble(
    feature = rownames(coef(object$fit, s = s)),
    importance = coef(object$fit, s = s)[, 1]
  )

  if (isFALSE(intercept))
    scores <- scores[scores$feature != "(Intercept)", ]

  scores
}


#' @export
pull_importances._randomForest <- function(object, scaled = FALSE, ...) {

  scores <- tibble::tibble(
    feature = rownames(object$fit$importance),
    importance = object$fit$importance
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


#' @export
pull_importances._rpart <- function(object, scaled = FALSE, ...) {

  scores <- tibble::tibble(
    feature = names(object$fit$variable.importance),
    importance = object$fit$variable.importance
  )

  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

  scores
}


# stan?
# surv?
