#' Parameter functions for feature selection recipes
#'
#' Feature selection recipes allow the top-performing features to be selected
#' using two parameters. `top_p` is for specifying the number of the
#' top-performing features.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#'   largest possible values, respectively.
#' @param trans A `trans` object from the `scales` package, such as
#'   `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#'   the default is used which matches the units used in `range`. If no
#'   transformation, `NULL`.
#'
#' @return A function with classes "quant_param" and "param"
#' @export
#'
#' @examples
#' top_p(c(3, 10))
top_p <- function(range = c(1L, 4L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(top_p = "# Selected Predictors"),
    finalize = dials::get_p
  )
}
