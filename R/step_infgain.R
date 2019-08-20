#' mRMR feature selection recipe
#'
#' Initial function - simple wrapper around add_step
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of operations
#'  for this recipe
#' @param ... One or more selector functions to choose which variables are affected by the
#'   step. See selections() for more details. For the tidy method, these are not currently
#'   used
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have been
#'   estimated
#' @param type character, one of c("infogain", "gainratio", "symuncert")
#' @param target character, name of response variable to use to evaluate information gain
#' value against the predictors
#' @param k numeric, if an integer value is supplied, then this represents the number of best
#' scoring features to select, if a decimal between 0 and 1 is supplied then then top percentile
#' of features are selected
#' @param threads integer, number of threads to use for processing, default = 0 uses all
#'   available threads
#' @param to_retain character, names of features to retain
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the outcome
#'   variable(s)). Care should be taken when using skip = TRUE as it may affect the
#'   computations for subsequent operations
#' @param id 	A character string that is unique to this step to identify it
#'
#' @return a step_mrmr object
#' @export
#' @importFrom recipes ellipse_check rand_id add_step
step_infgain <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  k = NULL,
  type = "infogain",
  threads = 1,
  to_retain = NULL,
  skip = FALSE,
  id = rand_id("infgain")) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_infgain_new(
      terms = terms,
      trained = trained,
      target = target,
      role = role,
      k = k,
      type = type,
      threads = threads,
      to_retain = to_retain,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @export
step_infgain_new <- function(terms, role, trained, target, k, type, threads, to_retain, skip, id) {
  step(
    subclass = "infgain",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    k = k,
    type = type,
    threads = threads,
    to_retain = to_retain,
    skip = skip,
    id = id
  )
}

# define the estimation procedure
# x is the step_mrmr object
# training is a tibble that has the training set data
# info is a tibble that contains information on the current set of data
# this is updated each time as each step function is evaluated by its prep method
#' @export
#' @importFrom FSelectorRcpp information_gain
#' @importFrom recipes terms_select
prep.step_infgain <- function(x, training, info = NULL, ...) {

  # first translate the terms argument into column name
  # this term should refer to the response variable for step_mrmr
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- x$target

  f <- as.formula(paste(target_name, "~", paste(col_names, collapse = "+")))

  # check for factors
  col_types <- sapply(training[, col_names], class, USE.NAMES = FALSE)
  if ("factor" %in% col_types) discIntegers = TRUE else discIntegers = FALSE

  ig_scores <- information_gain(
    formula = f, data = training,type = x$type, threads = x$threads, discIntegers = discIntegers, equal = TRUE)

  ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

  # select top scoring features
  if (is.null(x$k))
    x$k <- length(col_names)

  if (x$k %% 1 != 0)
    x$k <- ceiling(length(col_names) * x$k)

  to_retain  <- c(ig_scores[1:x$k, "attributes"], target_name)

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE
  step_infgain_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    k = x$k,
    type = x$type,
    threads = x$threads,
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
bake.step_infgain <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}
