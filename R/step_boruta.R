#' Boruta feature selection step
#'
#' `step_boruta` creates a *specification* of a recipe step that selects a
#' subset of predictors based on the scores of the information gain algorithm.
#' This step requires the FSelectorRcpp package to be installed. The top
#' `num_comp` scoring features, or features whose scores occur in the top
#' percentile `threshold` will be retained as new predictors
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param type Character, one of c("infogain", "gainratio", "symuncert").
#'   Default is 'infogain'.
#' @param target Name of response variable to use to evaluate information gain
#'   value against the predictors.
#' @param num_comp An integer with the number of best scoring features to
#'   select.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best features to select. For example, `threshold = 0.9` will retain only
#'   predictors with scores in the top 90th percentile. Note that this overrides
#'   num_comp.
#' @param threads An integer specifying the number of threads to use for
#'   processing. The default = 0 uses all available threads.
#' @param to_retain The names of features that will be retained. This parameter
#'   is NULL until the recipe is prepped.
#' @param scores A tibble containing the mRMR scores of predictors. This
#'   parameter is only produced after the recipe has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations
#' @param id 	A character string that is unique to this step to identify it.
#'
#' @return a step_boruta object.
#'
#' @export
#'
#' @importFrom recipes ellipse_check rand_id add_step
#' @importFrom rlang enquos
#'
#' @examples
#' library(recipes)
#' data("iris")
#' rec <- iris %>%
#'     recipe(Species ~.) %>%
#'     step_boruta(all_predictors(), target = Species, num_comp = 2)
#' prepped <- prep(rec)
#' new_data <- juice(prepped)
step_boruta <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  num_comp = NULL,
  threshold = NULL,
  type = "infogain",
  threads = 1,
  to_retain = NULL,
  scores = NULL,
  skip = FALSE,
  id = rand_id("boruta")) {

  if (!"FSelectorRcpp" %in% installed.packages()[, 1])
    stop("step_boruta requires the package `FSelectorRcpp` to be installed")

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_boruta_new(
      terms = terms,
      trained = trained,
      target = enquos(target),
      role = role,
      num_comp = num_comp,
      threshold = threshold,
      type = type,
      threads = threads,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_boruta_new <- function(terms, role, trained, target, num_comp, threshold,
                             type, threads, to_retain, scores, skip, id) {
  step(
    subclass = "boruta",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    num_comp = num_comp,
    threshold = threshold,
    type = type,
    threads = threads,
    to_retain = to_retain,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @importFrom recipes terms_select
#' @importFrom stats as.formula
#' @importFrom rlang eval_tidy call2
#' @importFrom tibble as_tibble
#' @export
prep.step_boruta <- function(x, training, info = NULL, ...) {

  # First translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- terms_select(x$target, info = info)

  # Perform IG
  f <- as.formula(paste(target_name, "~", paste0(col_names, collapse = " + ")))

  ig_call <- call2(
    .fn = "information_gain",
    .ns = "FSelectorRcpp",
    formula = f,
    data = training,
    type = x$type,
    threads = x$threads,
    discIntegers = TRUE,
    equal = FALSE
  )

  ig_scores <- eval_tidy(ig_call)
  ig_scores <- as_tibble(ig_scores)
  ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

  # Select top scoring features
  if (!is.null(x$threshold)) {
    score_to_exceed <- quantile(ig_scores$importance, c(x$threshold))
    x$num_comp <- max(which(ig_scores$importance >= score_to_exceed))
  }

  if (is.null(x$num_comp) & is.null(x$threshold))
    x$num_comp <- length(col_names)

  to_retain  <- c(ig_scores[1:x$num_comp, "attributes"], target_name)

  # Use the constructor function to return the updated object
  # Note that `trained` is set to TRUE
  step_boruta_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    num_comp = x$num_comp,
    threshold = x$threshold,
    type = x$type,
    threads = x$threads,
    to_retain = to_retain,
    scores = ig_scores,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom tibble as_tibble
#' @export
bake.step_boruta <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  # always convert to tibbles on the way out
  as_tibble(new_data)
}


#' @importFrom recipes format_ch_vec
#' @export
print.step_boruta <- function(x, width = max(20, options()$width - 40), ...) {
  if (x$trained) {
    if (x$num_comp == 0) {
      cat("No features were extracted.\n")
    } else {
      cat("Information gain importance (", x$ig_scores, ")")
      cat(format_ch_vec(colnames(x$ig_scores), width = width))
    }
  }
  if (x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


#' @export
tunable.step_boruta <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 4)),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_boruta",
    component_id = x$id
  )
}
