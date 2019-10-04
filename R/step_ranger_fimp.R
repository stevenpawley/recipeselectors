#' Select From Feature Importances (ranger)
#'
#' Feature selection step based on ranger feature importance scores
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations
#'  for this recipe
#' @param ... One or more selector functions to choose which variables are affected by the
#'   step. See selections() for more details. For the tidy method, these are not currently
#'   used
#' @param target character, name of response variable to use to evaluate information gain
#' value against the predictors
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have been
#'   estimated
#' @param model parsnip rand_forest model specification using the `ranger` engine
#' @param threshold numeric, percentile of top scoring features to retain
#' @param to_retain character, names of features to retain
#' @param scores tibble, tibble of feature importance scores
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the outcome
#'   variable(s)). Care should be taken when using skip = TRUE as it may affect the
#'   computations for subsequent operations
#' @param id A character string that is unique to this step to identify it
#'
#' @return a step_ranger_fimp object
#' @export
step_ranger_fimp <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  model = NULL,
  threshold = NULL,
  to_retain = NULL,
  scores = NULL,
  skip = FALSE,
  id = rand_id("fimp")) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_ranger_fimp_new(
      terms = terms,
      trained = trained,
      target = target,
      role = role,
      model = model,
      threshold = threshold,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @export
step_ranger_fimp_new <- function(terms, role, trained, target, model, threshold, to_retain, scores, skip,
                         id) {
  step(
    subclass = "fimp",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    model = model,
    threshold = threshold,
    to_retain = to_retain,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc
#' @importFrom stats quantile
prep.step_ranger_fimp <- function(x, training, info = NULL, ...) {

  # first translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- x$target

  # fit initial model and get feature importances
  f <- as.formula(paste(target_name, "~", paste(col_names, collapse = "+")))
  initial_model <- x$model %>% fit(f, training)

  feature_ranking <- initial_model$fit$variable.importance

  feature_ranking <- tibble(
    feature = names(feature_ranking),
    score = feature_ranking)

  feature_ranking <- feature_ranking %>% arrange(desc(score))

  # select k best features
  score_to_exceed <- quantile(feature_ranking$score, c(x$threshold))
  best_n <- which(feature_ranking$score >= score_to_exceed)
  to_retain <- c(feature_ranking[best_n, ][["feature"]], target_name)

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE
  step_ranger_fimp_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    model = x$model,
    threshold = x$threshold,
    to_retain = to_retain,
    scores = feature_ranking,
    skip = x$skip,
    id = x$id
  )
}

# prep method does not apply the method, it only calculates any required data
# the bake method is defined to do this
# object is the updated step function that has been through the corresponding prep code
# new_data is a tibble of data to be processed
#' @export
#' @importFrom tibble as_tibble
bake.step_ranger_fimp <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}
