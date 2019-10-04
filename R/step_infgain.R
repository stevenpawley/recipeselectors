#' Information gain feature selection step
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of operations for this
#'   recipe
#' @param ... One or more selector functions to choose which variables are affected by the step. See
#'   selections() for more details. For the tidy method, these are not currently used
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated
#' @param type character, one of c("infogain", "gainratio", "symuncert")
#' @param target character, name of response variable to use to evaluate information gain value
#'   against the predictors
#' @param num_comp numeric, if an integer value is supplied, then this represents the number of best
#'   scoring features to select
#' @param threads integer, number of threads to use for processing, default = 0 uses all available
#'   threads
#' @param to_retain character, names of features to retain
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake.recipe()?
#'   While all operations are baked when prep.recipe() is run, some operations may not be able to be
#'   conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when
#'   using skip = TRUE as it may affect the computations for subsequent operations
#' @param id 	A character string that is unique to this step to identify it
#'
#' @return a step_infgain object
#' @export
#' @importFrom recipes ellipse_check rand_id add_step
step_infgain <- function(
  recipe, ...,
  target = NULL,
  role = NA,
  trained = FALSE,
  num_comp = NULL,
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
      num_comp = num_comp,
      type = type,
      threads = threads,
      to_retain = to_retain,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_infgain_new <- function(terms, role, trained, target, num_comp, type, threads, to_retain, skip,
                             id) {
  step(
    subclass = "infgain",
    terms = terms,
    role = role,
    trained = trained,
    target = target,
    num_comp = num_comp,
    type = type,
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
#' @importFrom FSelectorRcpp information_gain
#' @importFrom recipes terms_select
#' @importFrom stats as.formula
#'
#' @export
prep.step_infgain <- function(x, training, info = NULL, ...) {

  # First translate the terms argument into column name
  col_names <- terms_select(terms = x$terms, info = info)
  target_name <- x$target

  f <- as.formula(paste(target_name, "~", paste(col_names, collapse = "+")))

  # Check for factors
  col_types <- sapply(training[, col_names], class, USE.NAMES = FALSE)
  if ("factor" %in% col_types) discIntegers = TRUE else discIntegers = FALSE

  ig_scores <- information_gain(
    formula = f, data = training, type = x$type, threads = x$threads,
    discIntegers = discIntegers, equal = TRUE)

  ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

  # Select top scoring features
  if (is.null(x$num_comp))
    x$num_comp <- length(col_names)

  to_retain  <- c(ig_scores[1:x$num_comp, "attributes"], target_name)

  # Use the constructor function to return the updated object
  # Note that `trained` is set to TRUE
  step_infgain_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    target = target_name,
    num_comp = x$num_comp,
    type = x$type,
    threads = x$threads,
    to_retain = to_retain,
    skip = x$skip,
    id = x$id
  )
}

#' prep method does not apply the method, it only calculates any required data.
#' The bake method is defined to do this.
#'
#' @param object is the updated step function that has been through the corresponding prep code
#' @param new_data is a tibble of data to be processed
#' @param ... currently unused
#'
#' @importFrom tibble as_tibble
#'
#' @export
bake.step_infgain <- function(object, new_data, ...) {

  new_data <- new_data[, (colnames(new_data) %in% object$to_retain)]

  # always convert to tibbles on the way out
  as_tibble(new_data)
}
