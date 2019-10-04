#' mRMR feature selection step
#'
#' Initial function - simple wrapper around add_step
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of operations for this
#'   recipe
#' @param ... One or more selector functions to choose which variables are affected by the step. See
#'   selections() for more details. For the tidy method, these are not currently used
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated
#' @param target character, name of response variable to evaluation MRMR against
#' @param num_comp numeric, if an integer value is supplied, then this represents the number of best
#'   scoring features to select, if a decimal between 0 and 1 is supplied then then top percentile
#'   of features are selected
#' @param threads integer, number of threads to use for processing, default = 0 uses all available
#'   threads
#' @param to_retain character, names of features to retain
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake.recipe()?
#'   While all operations are baked when prep.recipe() is run, some operations may not be able to be
#'   conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when
#'   using skip = TRUE as it may affect the computations for subsequent operations
#' @param id 	A character string that is unique to this step to identify it
#'
#' @return a step_mrmr object
#' @export
#' @importFrom recipes ellipse_check rand_id add_step
step_mrmr <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  num_comp = NULL,
  threads = 0,
  to_retain = NULL,
  skip = FALSE,
  id = rand_id("mrmr")) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_mrmr_new(
      terms = terms,
      trained = trained,
      target = target,
      role = role,
      num_comp = num_comp,
      threads = threads,
      to_retain = to_retain,
      skip = skip,
      id = id
    )
  )
}

# Wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_mrmr_new <- function(terms, role, trained, target, num_comp, threads, to_retain, skip, id) {
    step(
      subclass = "mrmr", # set class of new objects to 'step_mrmr'
      terms = terms,
      role = role,
      trained = trained,
      target = target,
      num_comp = num_comp,
      threads = threads,
      to_retain = to_retain,
      skip = skip,
      id = id
    )
  }

#' Define the estimation procedure
#'
#' @param x the step object
#'
#' @param training a tibble that has the training set data
#' @param info a tibble that contains information on the current set of data. This is updated each
#'   time as each step function is evaluated by its prep method
#' @param ... Currently unused
#'
#' @export
#' @importFrom praznik MRMR
#' @importFrom recipes terms_select
prep.step_mrmr <- function(x, training, info = NULL, ...) {

  # First translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- x$target

  # Perform mrmr using all features
  X <- training[, col_names]
  y <- training[[target_name]]

  # Some checks
  if (any(sapply(X, class)) == "factor")
    stop("mrmr step method cannot be applied to factors")

  # Perform MRMR using all features
  mi <- MRMR(X, y, length(col_names), x$threads)

  # Select top scoring features
  if (is.null(x$num_comp))
    x$num_comp <- length(col_names)

  to_retain  <- c(names(mi$selection)[1:x$num_comp], target_name)

  # Use the constructor function to return the updated object
  # Note that `trained` is set to TRUE
  step_mrmr_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    num_comp = x$num_comp,
    threads = x$threads,
    to_retain = to_retain,
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
bake.step_mrmr <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}
