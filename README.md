# recipeselectors

The goal of recipeselectors is to provide extra supervised feature selection
steps to be used with the tidymodels recipes package.

The package is under development.

## Installation

``` r
devtools::install_github("stevenpawley/recipeselectors")
```

## Feature Selection Methods

The following feature selection methods are implemented:

- `step_select_infgain` provides Information Gain feature selection. This step
requires the `FSelectorRcpp` package to be installed.

- `step_select_mrmr` provides maximum Relevancy Minimum Redundancy feature
selection. This step requires the `praznik` package to be installed.

- `step_select_roc` provides ROC-based feature selection based on each
predictors' relationship with the response outcomeas measured using a Receiver
Operating Characteristic curve. Thanks to Max Kuhn, along with many other useful
suggestions.

- `step_select_xtab` provides feature selection using statistical association
(also thanks to Max Kuhn).

- `step_select_vip` provides model-based selection using feature importance
scores or coefficients. This method allows a `parsnip` model specification to be
used to select a subset of features based on the models' feature importances or
coefficients. See below for details.

## Under Development

Methods that are planned to be added:

- Boruta feature selection (Boruta package)

- Carscore (care package)

- Relief-based methods (CORElearn package)

- Ensemble feature selection (EFS package)

Considering:

- Decision tree feature selection (with tunable arguments)

- Random forest feature selection (with tunable arguments)

- Boosted trees feature selection (with tunable arguments)

## Notes on Wrapper Feature Selection Methods

The focus of `recipeselectors` is to provide extra recipes for filter-based 
feature selection. A single wrapper method is also included using the variable
importance scores of selected algorithms for feature selection.

The `step_select_vip` is designed to work with the `parsnip` package and
requires a base model specification that provides a method of ranking the
importance of features, such as feature importance scores or coefficients, with
one score per feature. The base model is specified in the step using the `model`
parameter.

A limitation is that the model used in the `step_select_vip` cannot be tuned.
This step is likely to be superceded in the future with a more appropriate
structure that allows both variable selection and tuning. 

The parsnip package does not currently contain a method of pulling feature 
importance scores from models that support them. The `recipeselectors` package
provides a generic function `pull_importances` for this purpose that accepts
a fitted parsnip model, and returns a tibble with two columns 'feature' and
'importance':

```
model <- boost_tree(mode = "classification") %>%
  set_engine("xgboost")

model_fit <- model %>% 
  fit(Species ~., iris)

pull_importances(model_fit)
```

Most of the models and 'engines' that provide feature importances are
implemented. In addition, `h2o` models are supported using the `h2oparsnip`
package. Use `methods(pull_importances)` to list models that are currently
implemented. If need to pull the feature importance scores from a model that is
not currently supported in this package, then you can add a class to the
pull_importances generic function which returns a two-column tibble:

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
