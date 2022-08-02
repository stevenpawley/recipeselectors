# colino

The goal of colino is to provide supervised feature selection steps to be used
with the tidymodels recipes package.

Note - colino is the new package name and replaces the preliminary
'recipeselectors' name. Colino will be submitted to CRAN once some additional
steps and documentation have been finalized.

## Installation

``` r
devtools::install_github("stevenpawley/colino")
```

## Feature Selection Methods

The following feature selection methods are implemented:

- `step_select_infgain` provides Information Gain feature selection. This step
requires the `FSelectorRcpp` package to be installed and can be used for both
classification and regression problems. For regression, the target variable is
discretized using equal frequency binning. The number of bins can be set with
the'options' parameter (default is `options = list(nbins = 5)`).

- `step_select_mrmr` provides maximum Relevancy Minimum Redundancy feature
selection. This step requires the `praznik` package to be installed. This step
can be used for classification and regression problems. Similar to information
gain, binning is used when the target variable is continuous.

- `step_select_roc` provides ROC-based feature selection based on each
predictors' relationship with the response outcomes measured using a Receiver
Operating Characteristic curve.

- `step_select_xtab` provides feature selection using statistical association.

- `step_select_vip` provides model-based selection using feature importance
scores or coefficients. This method allows a `parsnip` model specification to be
used to select a subset of features based on the models' feature importances or
coefficients. See below for details.

- `step_select_boruta` provides a Boruta feature selection step. This step can
be used for classification and regression problems.

- `step_select_carscore` provides a CAR score feature selection step for
regression models. This step requires the `care` package to be installed.

- `step_select_relief` provides a Relief-based feature selection step for
classification and regression models. This step requires the `FSelectorRcpp`
package to be installed.

- `step_select_forests`, `step_select_tree`, and `step_select_linear` provide
model-based methods of selecting a subset of features based on the model's
feature importance scores or coefficients.

## Notes

The focus of `colino` is to provide extra recipes for filter-based 
feature selection.

The `step_select_vip` is designed to work with the `parsnip` package and
requires a base model specification that provides a method of ranking the
importance of features, such as feature importance scores or coefficients, with
one score per feature. The base model is specified in the step using the `model`
parameter.

Although `step_select_vip` allows a diverse range of models to be used as the
ranking algorithm, and potentially allows new models to be implemented, a
limitation is that the hyperparameters of the ranking model cannot be tuned. As
an alternative, `step_select_linear`, `step_select_tree` and
`step_select_forests` provide steps specific to these types of models where the
hyperparameters of ranking model can be tuned using the same tuning arguments as
`parsnip`.

The parsnip package does not currently contain a method of pulling feature
importance scores from models that support them. The `colino` package provides a
generic function `pull_importances` for this purpose that accepts a fitted
parsnip model, and returns a tibble with two columns 'feature' and 'importance':

```
model <- boost_tree(mode = "classification") %>%
  set_engine("xgboost")

model_fit <- model %>% 
  fit(Species ~., iris)

pull_importances(model_fit)
```

Most of the models and 'engines' that provide feature importances are
implemented. In addition, `h2o` models are supported using the `agua` package.
Use `methods(pull_importances)` to list models that are currently implemented.
If need to pull the feature importance scores from a model that is not currently
supported in this package, then you can add a class to the pull_importances
generic function which returns a two-column tibble:

```
pull_importances._ranger <- function(object, scaled = FALSE, ...) {
  scores <- ranger::importance(object$fit)

  # create a tibble with 'feature' and 'importance' columns
  scores <- tibble::tibble(
    feature = names(scores),
    importance = as.numeric(scores)
  )

  # optionally rescale the importance scores
  if (scaled)
    scores$importance <- scales::rescale(scores$importance)
  scores
}
```

An example of using the step_importance function:

```
library(parsnip)
library(recipes)
library(magrittr)

# load the example iris dataset
data(iris)

# define a base model to use for feature importances
base_model <- rand_forest(mode = "classification") %>%
  set_engine("ranger", importance = "permutation")

# create a preprocessing recipe
rec <- iris %>%
recipe(Species ~ .) %>%
step_select_vip(all_predictors(), model = base_model, top_p = 2,
                outcome = "Species")

prepped <- prep(rec)

# create a model specification
clf <- decision_tree(mode = "classification") %>%
set_engine("rpart")

clf_fitted <- clf %>%
  fit(Species ~ ., juice(prepped))
```
