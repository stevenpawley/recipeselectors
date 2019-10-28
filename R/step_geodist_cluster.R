#' Information gain feature selection step
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe
#' @param lat selector for variable to represent y coordinate data
#' @param lon select for variable to represent x coordinate data
#' @param role Not used by this step since no new variables are created
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated
#' @param num_comp numeric, the number of clusters to spatially segregate the
#' training data into
#' @param columns names for the x, y coordinates for the cluster centroids
#' @param ref_lons numeric, x coordinates of the cluster centroids
#' @param ref_lats numeric, y coordinates of the cluster centroids
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations
#' @param id 	A character string that is unique to this step to identify it
#'
#' @return a step_infgain object
#' @export
#' @importFrom recipes ellipse_check rand_id add_step
#' @importFrom rlang enquos
step_geodist_cluster <- function(
  recipe,
  lat = NULL,
  lon = NULL,
  role = "predictor",
  num_comp = 5,
  trained = FALSE,
  columns = NULL,
  ref_lons = NULL,
  ref_lats = NULL,
  skip = FALSE,
  id = rand_id("geodist_cluster")) {

  add_step(
    recipe,
    step_geodist_cluster_new(
      lat = enquo(lat),
      lon = enquo(lon),
      role = role,
      num_comp = num_comp,
      trained = trained,
      columns = columns,
      ref_lons = ref_lons,
      ref_lats = ref_lats,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_geodist_cluster_new <- function(lon, lat, role, num_comp,
                                     trained, columns, ref_lons, ref_lats, skip,
                                     id) {
  step(
    subclass = "geodist_cluster",
    lon = lon,
    lat = lat,
    role = role,
    num_comp = num_comp,
    trained = trained,
    columns = columns,
    ref_lons = ref_lons,
    ref_lats = ref_lats,
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
#' @importFrom recipes terms_select check_type
#' @importFrom stats as.formula
#' @importFrom rlang eval_tidy set_names
#' @importFrom tibble as_tibble
#'
#' @export
prep.step_geodist_cluster <- function(x, training, info = NULL, ...) {

  lon_name <- terms_select(x$lon, info = info)

  if (length(lon_name) > 1)
    stop("`lon` should resolve to a single column name.", call. = FALSE)
  check_type(training[, lon_name])
  lat_name <- terms_select(x$lat, info = info)

  if (length(lat_name) > 1)
    stop("`lat` should resolve to a single column name.", call. = FALSE)
  check_type(training[, lat_name])

  if (x$num_comp > 0) {
    km <- kmeans(
      training[, c(lon_name, lat_name)],
      centers = x$num_comp,
      iter.max = 10000,
      algorithm = "Lloyd")

    clusters <-
      as_tibble(km$centers) %>%
      set_names(c("x_crd", "y_crd"))

    ref_lats <- clusters[["y_crd"]]
    ref_lons <- clusters[["x_crd"]]

  } else {
    ref_lats <- NULL
    ref_lons <- NULL
  }

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE
  step_geodist_cluster_new(
    lon = x$lon,
    lat = x$lat,
    role = x$role,
    num_comp = x$num_comp,
    trained = TRUE,
    columns = c(lat_name, lon_name),
    ref_lons = ref_lons,
    ref_lats = ref_lats,
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
#' @importFrom dplyr bind_cols
#' @importFrom purrr map2_dfc
#'
#' @export
bake.step_geodist_cluster <- function(object, new_data, ...) {

  if (object$num_comp > 0) {

    dist_vals <- map2_dfc(
      object$ref_lons, object$ref_lats,
      ~ recipes:::geo_dist_calc(new_data[, object$columns], .x, .y)
    )

    names(dist_vals) <- paste0("buffer", 1:object$num_comp)

    new_data <- bind_cols(new_data, dist_vals)
  }

  new_data
}


#' Specify tunable arguments of step
#'
#' @param x step
#' @param ... currently unused
#'
#' @return tibble
#' @export
tunable.step_geodist_cluster <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 25))
    ),
    source = "recipe",
    component = "step_geodist_cluster",
    component_id = x$id
  )
}
