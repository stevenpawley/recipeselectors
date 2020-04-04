neighbor_idw <- function(x, y, field, k, idp = 2, metrics) {
  # calculate nearest points
  call <-
    rlang::call2(
      "st_nn",
      x = x,
      y = y,
      k = k + 1,
      returnDist = TRUE,
      progress = FALSE,
      .ns = "nngeo"
    )

  dists <- rlang::eval_tidy(call)

  # get indices of nearest points
  neighbor_ids <-
    matrix(unlist(dists$nn), nrow = length(dists$nn), byrow = TRUE)
  neighbor_ids <- neighbor_ids[, 2:ncol(neighbor_ids)]

  # get matrix of values of nearest points
  neighbor_vals <- y[as.numeric(neighbor_ids), ][[field]]
  neighbor_vals <- matrix(neighbor_vals, ncol = k)

  # get matrix of distances to nearest points
  neighbor_dists <-
    matrix(unlist(dists$dist), nrow = length(dists$dist), byrow = TRUE)
  neighbor_dists <- neighbor_dists[, 2:ncol(neighbor_dists)]

  # summary metrics
  df <- tibble(.rows = nrow(neighbor_vals))

  # inverse distance weighted mean
  if ("idw" %in% metrics) {
    neighbor_weights <- 1 / neighbor_dists^idp
    neighbor_weights[is.infinite(neighbor_weights)] <- 1

    idw <- sapply(1:nrow(neighbor_vals), function(i)
      weighted.mean(neighbor_vals[i, ], neighbor_weights[i, ]))

    df$idw <- idw
  }

  # unweighted mean
  if ("mean" %in% metrics) {
    mean_v <- apply(neighbor_vals, 1, mean)
    df$mean_neighbors <- mean_v
  }

  # standard deviation
  if ("sd" %in% metrics) {
    sd_v <- apply(neighbor_vals, 1, sd)
    df$sd_neighbors <- sd_v
  }

  # minimum value
  if ("min" %in% metrics) {
    min_v <- apply(neighbor_vals, 1, min)
    df$min_neighbors <- min_v
  }

  # maximum value
  if ("max" %in% metrics) {
    max_v <- apply(neighbor_vals, 1, max)
    df$max_neighbors <- max_v
  }

  # raw values of neighbors
  if ("raw" %in% metrics) {
    colnames(neighbor_vals) <- paste0(field, "_neighbors", 1:k)
    colnames(neighbor_dists) <- paste0(field, "_neighbors_dists", 1:k)
    df <- bind_cols(df, as.data.frame(neighbor_vals), as.data.frame(neighbor_dists))
  }

  df
}


#' Inverse distance-weighting interpolation
#'
#' `step_idw` creates a *specification* of a recipe step that will add a new
#' 'idw' feature to a dataset based on the inverse distance-weighted mean of
#' surrounding observations.
#'
#' @param recipe A recipe.
#' @param target Selector function to choose which variable will be used to
#'   create a new feature based on the inverse distance-weighted mean of
#'   surrounding observations.
#' @param lon,lat Selector functions to indicate which variables should be used
#' to calculate the distance measures to the closest observations.
#' @param role role or model term created by this step, what analysis
#'  role should be assigned?. By default, the function assumes
#'  that resulting distance will be used as a predictor in a model.
#' @param trained A logical that will be updated once the step has been trained.
#' @param neighbors The number of closest neighbours to use in the distance
#'   weighting.
#' @param dist_power Distance weighting power. The distance-based weights are
#'   raised to this power. Values > 1 cause very proximal observations to be
#'   weighted more highly.
#' @param metrics Summary metrics to produce. One or multiple of c("idw",
#'   "mean", "sd", "raw).
#' @param spatial Used internally to store an `sf` object of the spatial
#'   locations.
#' @param columns Used internally to store the names of the spatial columns.
#' @param skip A logical to skip training.
#' @param id An identifier for the step. If omitted then this is generated
#' automatically.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns echoing the values of `lat`, `lon`, `neighbors`, `dist_power`, and
#'   `id`.
#' @export
#'
#' @examples
#' library(gstat)
#' library(sf)
#' library(tidymodels)
#'
#' data("meuse.all")
#'
#' regr <- rand_forest(mode = "regression", min_n = 1) %>%
#'     set_engine("ranger")
#'
#' rec <- meuse.all %>%
#'     select(x, y, zinc, elev, dist.m, soil) %>%
#'     recipe(zinc ~ .) %>%
#'     step_idw(lon = x, lat = y, target = zinc, neighbors = 3, dist_power = 2)
step_idw <- function(
  recipe,
  target = NULL,
  lon = NULL,
  lat = NULL,
  role = "predictor",
  trained = FALSE,
  neighbors = NULL,
  dist_power = NULL,
  metrics = "idw",
  spatial = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("idw")) {

  if (neighbors <= 0)
    rlang::abort("`neighbors` should be greater than 0.")
  if (dist_power <= 0)
    rlang::abort("`dist_power` should be greater than 0.")

  add_step(
    recipe,
    step_idw_new(
      lon = enquos(lon),
      lat = enquos(lat),
      target = enquos(target),
      trained = trained,
      role = role,
      neighbors = neighbors,
      dist_power = dist_power,
      metrics = metrics,
      spatial = spatial,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_idw_new <- function(role, lat, lon, trained, target, neighbors, dist_power,
                         metrics, spatial, columns, skip, id) {
  step(
    subclass = "idw",
    lon = lon,
    lat = lat,
    role = role,
    trained = trained,
    target = target,
    neighbors = neighbors,
    dist_power = dist_power,
    metrics = metrics,
    spatial = spatial,
    columns = columns,
    skip = skip,
    id = id
  )
}


prep.step_idw <- function(x, training, info = NULL, ...) {

  # First translate the terms argument into column name
  target_name <- terms_select(x$target, info = info)

  lon_name <- terms_select(x$lon, info = info)
  if (length(lon_name) > 1)
    rlang::abort("`lon` should resolve to a single column name.")
  check_type(training[, lon_name])

  lat_name <- terms_select(x$lat, info = info)
  if (length(lat_name) > 1)
    rlang::abort("`lat` should resolve to a single column name.")
  check_type(training[, lat_name])

  # Use the constructor function to return the updated object
  # Note that `trained` is set to TRUE
  step_idw_new(
    lon = x$lon,
    lat = x$lat,
    role = x$role,
    trained = TRUE,
    target = target_name,
    neighbors = x$neighbors,
    dist_power = x$dist_power,
    metrics = x$metrics,
    spatial = st_as_sf(training, coords = c(lon_name, lat_name)),
    columns = c(lon_name, lat_name),
    skip = x$skip,
    id = x$id
  )
}


bake.step_idw <- function(object, new_data, ...) {

  new_data_sf <- st_as_sf(new_data, coords = object$columns)

  w <- neighbor_idw(
    x = new_data_sf,
    y = object$spatial,
    field = object$target,
    k = object$neighbors,
    idp = object$dist_power,
    metrics = object$metrics
  )
  new_data <- bind_cols(new_data, w)
}


#' @importFrom recipes format_ch_vec
print.step_idw <- function(x, width = max(20, options()$width - 40), ...) {
  cat("Step parameters",
      x$neighbors, "neighbors",
      x$dist_power, "dist_power"
  )
  invisible(x)
}


tidy.step_idw <- function(x, ...) {
  res <- tibble(
    neighbors = x$neighbors,
    dist_power = x$dist_power,
    id = x$id
  )
  res
}


tunable.step_idw <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "dist_power"),
    call_info = list(
      list(pkg = "dials", fun = "neighbors", range = c(1, 10)),
      list(pkg = "dials", fun = "dist_power", range = c(1, 3))
    ),
    source = "recipe",
    component = "step_idw",
    component_id = x$id
  )
}
