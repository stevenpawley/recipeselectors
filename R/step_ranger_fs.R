#' Select From Feature Importances (ranger)
#'
#' Feature selection step based on ranger feature importance scores
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used
#' @param target name of response variable to use to evaluate information gain
#'   value against the predictors
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated
#' @param trees numeric, number of trees in the forest, default is 100
#' @param importance character, one of `impurity`, `impurity_corrected`,
#'   `permutation`, default is `permutation`
#' @param splitrule character, one of `gini`, `variance`, `extratrees`,
#'   `maxstat`
#' @param min_n numeric, default is 1 for classification, 5 for regression, and
#'   10 for probability
#' @param num_comp numeric, the number of best scoring features to select
#' @param to_retain character, names of features to retain
#' @param scores tibble, tibble of feature importance scores
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it
#'
#' @return a step_ranger_fs object
#' @importFrom recipes recipes_pkg_check add_step
#' @export
step_ranger_fs <- function(
  recipe, ...,
  target = NULL,
  role = "predictor",
  trained = FALSE,
  trees = 100,
  importance = "permutation",
  splitrule = "gini",
  min_n = NULL,
  num_comp = NULL,
  to_retain = NULL,
  scores = NULL,
  skip = FALSE,
  id = rand_id("ranger_fs")) {

  recipes_pkg_check("ranger")

  add_step(
    recipe,
    step_ranger_fs_new(
      terms = ellipse_check(...),
      trained = trained,
      target = enquos(target),
      role = role,
      trees = trees,
      importance = importance,
      splitrule = splitrule,
      min_n = min_n,
      num_comp = num_comp,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_ranger_fs_new <- function(terms, role, trained, target, trees,
                               importance, splitrule, min_n, num_comp,
                               to_retain, scores, skip, id) {
  step(
    subclass = "ranger_fs",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    trees = trees,
    importance = importance,
    splitrule = splitrule,
    min_n = min_n,
    num_comp = num_comp,
    to_retain = to_retain,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' Define the estimation procedure
#'
#' @param x the step object
#'
#' @param training a tibble that has the training set data
#' @param info a tibble that contains information on the current set of data.
#'   This is updated each time as each step function is evaluated by its prep
#'   method
#' @param ... Currently unused
#'
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc
#' @importFrom stats quantile
#' @importFrom recipes terms_select check_type
#' @importFrom parsnip rand_forest set_engine set_mode fit_xy
#' @importFrom rlang eval_tidy enquos
#'
#' @export
prep.step_ranger_fs <- function(x, training, info = NULL, ...) {

  # first translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- terms_select(x$target, info = info)

  check_type(training[, col_names])

  # fit initial model and get feature importances
  X <- training[, col_names]
  y <- training[[target_name]]

  if (inherits(y, "numeric")) {
    mode <- "regression"
  } else {
    mode <- "classification"
  }

  model <-
    rand_forest(trees = x$trees, min_n = x$min_n) %>%
    set_mode(mode = mode) %>%
    set_engine("ranger", num.threads = x$num.threads, importance = x$importance,
               splitrule = x$splitrule)

  initial_model <- model %>% fit_xy(X, y)

  feature_ranking <- initial_model$fit$variable.importance

  feature_ranking <- tibble(
    feature = names(feature_ranking),
    score = feature_ranking)

  feature_ranking <- feature_ranking %>% arrange(desc(!!sym("score")))

  # select k best features
  if (is.null(x$num_comp))
    x$num_comp <- length(col_names)

  to_retain <- c(feature_ranking[1:x$num_comp, ][["feature"]], target_name)

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE
  step_ranger_fs_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    trees = x$trees,
    importance = x$importance,
    splitrule = x$splitrule,
    min_n = x$min_n,
    num_comp = x$num_comp,
    to_retain = to_retain,
    scores = feature_ranking,
    skip = x$skip,
    id = x$id
  )
}

#' prep method does not apply the method, it only calculates any required data.
#' The bake method is defined to do this.
#'
#' @param object is the updated step function that has been through the
#'   corresponding prep code
#' @param new_data is a tibble of data to be processed
#' @param ... currently unused
#'
#' @importFrom tibble as_tibble
#'
#' @export
bake.step_ranger_fs <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}

#' @importFrom recipes format_ch_vec
print.step_ranger_fs <- function(x, width = max(20, options()$width - 40), ...) {
  if (x$trained) {
    if (x$num_comp == 1) {
      cat("No features were extracted.\n")
    } else {
      cat("Ranger feature importance (",
          x$model$fit$importance.mode,
          ") extraction with ",
          sep = "")
      cat(format_ch_vec(
        colnames(x$model$fit$variable.importance), width = width))
    }
  }
  if (x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


#' Specify tunable arguments of step
#'
#' @param x step
#' @param ... currently unused
#'
#' @return tibble
#' @export
tunable.step_ranger_fs <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp")
    ),
    source = "recipeselectors",
    component = "step_ranger_fs",
    component_id = x$id
  )
}
