rescale <- function(x)
  (x-min(x))/(max(x) - min(x)) * 100


#' Pull feature importances from a `model_fit` object
#'
#' @param object A `model_fit` object.
#' @param ... A list of other parameters passed to the feature importance
#'   method.
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' model <- boost_tree(mode = "classification") %>%
#' set_engine("xgboost")
#' model_fit <- model %>% fit(Species ~., iris)
#' pull_importances(model_fit)
#' }
pull_importances <- function(object, ...) {
  UseMethod("pull_importances", object)
}


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

