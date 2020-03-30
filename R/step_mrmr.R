#' Apply minimum Redundancy Maximum Relevance Feature Selection (mRMR)
#'
#' `step_mrmr` creates a *specification* of a recipe step that will apply
#' minimum Redundancy Maximum Relevance Feature Selection (mRMR) to numeric
#' data. The top `num_comp` scoring features, or features whose scores occur
#' in the top percentile `threshold` will be retained as new predictors.
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated
#' @param target Name of response variable used to evaluate the mRMR against.
#' @param num_comp The number of best scoring features to select.
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
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id 	A character string that is unique to this step to identify it.
#'
#' @return a step_mrmr object.
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
#'     step_mrmr(all_predictors(), target = Species, num_comp = 2)
#' prepped <- prep(rec)
#' new_data <- juice(prepped)
step_mrmr <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  num_comp = NULL,
  threshold = NULL,
  threads = 0,
  to_retain = NULL,
  scores = NULL,
  skip = FALSE,
  id = rand_id("mrmr")) {

  if (!"praznik" %in% installed.packages()[, 1])
    stop("step_infgain requires the package `FSelectorRcpp` to be installed")

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_mrmr_new(
      terms = terms,
      trained = trained,
      target = enquos(target),
      role = role,
      num_comp = num_comp,
      threshold = threshold,
      threads = threads,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# Wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_mrmr_new <- function(terms, role, trained, target, num_comp, threshold,
                          threads, to_retain, scores, skip, id) {
    step(
      subclass = "mrmr", # set class of new objects to 'step_mrmr'
      terms = terms,
      role = role,
      trained = trained,
      target = target,
      num_comp = num_comp,
      threshold = threshold,
      threads = threads,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  }


#' Define the estimation procedure
#'
#' @param x The step object.
#'
#' @param training A tibble that has the training set data.
#' @param info A tibble that contains information on the current set of data.
#'   This is updated each time as each step function is evaluated by its prep
#'   method.
#' @param ... Currently unused
#'
#' @importFrom recipes terms_select
#' @importFrom rlang call2 eval_tidy
#'
#' @export
prep.step_mrmr <- function(x, training, info = NULL, ...) {

  # First translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- terms_select(x$target, info = info)

  # Perform mrmr using all features
  X <- training[, col_names]
  y <- training[[target_name]]

  # Perform MRMR using all features
  call <- rlang::call2(
    .fn = "MRMR",
    .ns = "praznik",
    X = X,
    Y = y,
    k = length(col_names),
    threads = x$threads
  )

  res <- eval_tidy(call)

  mrmr_tbl <- tibble(
    selection = res$selection,
    score = res$score,
    attribute = names(res$selection))

  # Select top scoring features
  if (!is.null(x$threshold)) {
    score_to_exceed <- quantile(mrmr_tbl$score, c(x$threshold))
    x$num_comp <- max(which(mrmr_tbl$score >= score_to_exceed))
  }

  if (is.null(x$num_comp) & is.null(x$threshold))
    x$num_comp <- length(col_names)

  to_retain <- c(mrmr_tbl[1:x$num_comp, ][["attribute"]], target_name)

  # Use the constructor function to return the updated object
  # Note that `trained` is set to TRUE
  step_mrmr_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    num_comp = x$num_comp,
    threshold = x$threshold,
    threads = x$threads,
    to_retain = to_retain,
    scores = mrmr_tbl,
    skip = x$skip,
    id = x$id
  )
}


#' The prep method does not apply the method, it only calculates any required
#' data. The bake method is defined to do this.
#'
#' @param object The updated step function that has been through the
#'   corresponding prep code.
#' @param new_data A tibble of data to be processed.
#' @param ... Currently unused.
#'
#' @importFrom tibble as_tibble
#'
#' @export
bake.step_mrmr <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}

#' @importFrom recipes format_ch_vec
print.step_mrmr <- function(x, width = max(20, options()$width - 40), ...) {
  if (x$trained) {
    if (x$num_comp == 0) {
      cat("No features were extracted.\n")
    } else {
      cat("Information gain importance")
      cat(format_ch_vec(colnames(x$scores), width = width))
    }
  }
  if (x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


#' Specify tunable arguments of step
#'
#' @param x step.
#' @param ... Currently unused.
#'
#' @return A tibble.
#' @export
tunable.step_mrmr <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 10)),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_mrmr",
    component_id = x$id
  )
}
