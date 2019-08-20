# test_preproc <- preproc$terrain
#
# test_preproc <- test_preproc %>% step_rfe(
#   all_predictors(),
#   target = "pick_thk_m",
#   incr = 3,
#   model = regr_et_rfe %>% set_args(min_n = 1, trees = 10))
#
# test <- test_preproc %>% prep()
# test %>% juice()
#
# regr_et %>%
#   set_args(min_n = 1, trees = 50, mtry = .cols()) %>%
#   fit(formula(test),
#       test %>% juice())


step_rfe <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  model = NULL,
  incr = 1,
  type = "rfe",
  to_retain = NULL,
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
      type = type,
      to_retain = to_retain,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @export
step_rfe_new <- function(terms, role, trained, target, model, incr, type, to_retain, skip, id) {
  step(
    subclass = "rfe",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    model = model,
    incr = incr,
    type = type,
    to_retain = to_retain,
    skip = skip,
    id = id
  )
}


prep.step_rfe <- function(x, training, info = NULL, ...) {

  # first translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- x$target

  # fit initial model and get feature importances
  f <- as.formula(paste(target_name, "~", paste(col_names, collapse = "+")))

  initial_model <- x$model %>%
    set_args(mtry = length(formula.tools::rhs.vars(f))) %>%
    fit(f, training)

  feature_ranking <- initial_model$fit$variable.importance

  feature_ranking <- tibble(
    predictor = names(feature_ranking),
    importance = feature_ranking)

  feature_ranking <- feature_ranking %>% arrange(desc(importance))

  # rfe using ranger
  fea_incr <- seq(length(col_names), 1, by = -x$incr)

  scores <- sapply(fea_incr, function(n) {

    selected_feature_names <- feature_ranking[1:n, ][["predictor"]]
    selected_feature_names <- paste0(selected_feature_names, collapse = "+")
    f_selected <- as.formula(paste0(paste0(target_name, " ~ "), selected_feature_names))

    n_var <- length(formula.tools::rhs.vars(f_selected))

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
  best_n <- scores[which.min(scores$score), ][["n"]]
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
    type = x$type,
    to_retain = to_retain,
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
bake.step_rfe <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}
