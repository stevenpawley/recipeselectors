#' Recursive Feature Elimation using `ranger``
#'
#' Feature selection step using random forests recursive feature
#' elimination.
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
#' @param incr integer, increment step size to use in recursive feature elimination, default is 1
#' @param tol numeric, tolerance to use when selecting the best subset of features that results in
#' the lowest error score (r2 for regression models, fraction of missclassified for classification
#' models). A tol value of 0 will result in the optimal subset of features being selected based on
#' those that produce the lowest out-of-bag error score. A tol value > 0 will select the smallest
#' group of features that are within tol of the lowest score.
#' @param to_retain character, names of features to retain
#' @param scores tibble, tibble of scores per remaining features
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the outcome
#'   variable(s)). Care should be taken when using skip = TRUE as it may affect the
#'   computations for subsequent operations
#' @param id A character string that is unique to this step to identify it
#'
#' @return a step_rfe object
#' @export
step_rfe <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  model = NULL,
  incr = 1,
  tol = 0,
  to_retain = NULL,
  scores = NULL,
  skip = FALSE,
  id = rand_id("rfe")) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_rfe_new(
      terms = terms,
      trained = trained,
      target = target,
      role = role,
      model = model,
      incr = incr,
      tol = tol,
      to_retain = to_retain,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}

# Wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_rfe_new <- function(terms, role, trained, target, model, incr, tol, to_retain, scores, skip,
                         id) {
  step(
    subclass = "rfe",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    model = model,
    incr = incr,
    tol = tol,
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
#' This is updated each time as each step function is evaluated by its prep method
#' @param ... Currently unused
#'
#' @export
#' @importFrom formula.tools rhs.vars
#' @importFrom tibble tibble
#' @importFrom parsnip set_args fit
#' @importFrom dplyr arrange desc
#' @importFrom rlang sym
prep.step_rfe <- function(x, training, info = NULL, ...) {

  # first translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- x$target

  # fit initial model and get feature importances
  f <- as.formula(paste(target_name, "~", paste(col_names, collapse = "+")))

  initial_model <- x$model %>%
    set_args(mtry = length(rhs.vars(f))) %>%
    fit(f, training)

  feature_ranking <- initial_model$fit$variable.importance

  feature_ranking <- tibble(
    predictor = names(feature_ranking),
    importance = feature_ranking)

  feature_ranking <- feature_ranking %>% arrange(desc(!! sym("importance")))

  # rfe using ranger
  fea_incr <- seq(length(col_names), 1, by = -x$incr)

  scores <- sapply(fea_incr, function(n) {

    selected_feature_names <- feature_ranking[1:n, ][["predictor"]]
    selected_feature_names <- paste0(selected_feature_names, collapse = "+")
    f_selected <- as.formula(paste0(paste0(target_name, " ~ "), selected_feature_names))

    n_var <- length(rhs.vars(f_selected))

    rfe_model <- x$model %>%
      set_args(mtry = n_var) %>%
      fit(f_selected, training)

    rfe_model$fit$prediction.error
  })

  scores <- tibble(
    n = fea_incr,
    score = scores
  )

  # select k best features
  if (x$tol == 0) {
    best_n <- scores[which.min(scores$score), ][["n"]]
  } else {
    best_n <- min(which(scores$score < min(scores$score) + x$tol))
  }
  to_retain <- c(feature_ranking[1:best_n, ][["predictor"]], target_name)


  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE
  step_rfe_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    model = x$model,
    incr = x$incr,
    tol = x$tol,
    to_retain = to_retain,
    scores = scores,
    skip = x$skip,
    id = x$id
  )
}

#' bake method to apply the method from prep to new_data
#'
#' @param object is the updated step function that has been through the corresponding prep code
#'
#' @param new_data is a tibble of data to be processed
#' @param ... currently unused
#'
#' @export
#' @importFrom tibble as_tibble
bake.step_rfe <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}
