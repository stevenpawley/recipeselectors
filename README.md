
# recipeselectors

The goal of recipeselectors is to provide feature selection steps to be used
with the tidymodels recipes package.

The package is under development, although the currently implemented steps
are functional.

## Installation

``` r
devtools::install_github("stevenpawley/recipeselectors")
```

## Feature Selection Methods

The following feature selection methods are implemented:

- Information Gain via the `step_infgain` function. This step requires the 
`FSelectorRcpp` package to be installed.
- maximum Relevancy Minimum Redundancy via the `step_mrmr` function. This step
requires the `praznik` package to be installed.
- Model-based selection from feature importances or coefficients via the
`step_importance` function. This method allows a `parsnip` model specification to
be used to select a subset of features based on the models' feature importances
or coefficients. See below for details.

Methods that are planned to be added:

- Selection using permutation importance scores
- Boruta feature selection
- Univariate feature selection methods (Chi-squared, ANOVA f-value, linear model)

## Feature importances based selection

The `step_importance` is designed to work with the `parsnip` package and
requires a base model specification that provides a method of ranking the
importance of features, such as feature importance scores or coefficients, with
one score per feature. The base model is specified in the step using the `model`
parameter.

The parsnip package does not currently contain a method of pulling feature 
importance scores from models that support them. The `recipeselectors` package
provides a generic function `pull_importances` for this purpose that accepts
a fitted parsnip model, and returns a tibble with two columns 'feature' and
'importance':

```
model <- boost_tree(mode = "classification") %>%
set_engine("xgboost")
model_fit <- model %>% fit(Species ~., iris)
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

  # create a call to the ranger::importance function. This avoids having to use
  # ranger as a dependency in the package. For your own code you could call the
  # function directly
  call <- rlang::call2(
    .fn = "importance",
    .ns = "ranger",
    x = object$fit
  )

  scores <- rlang::eval_tidy(call)

  # create a tibble with 'feature' and 'importance' columns
  scores <- tibble::tibble(
    feature = names(scores),
    importance = as.numeric(scores)
  )

  # optionally rescale the importance scores
  if (isTRUE(scaled))
    scores$importance <- rescale(scores$importance)

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
step_importance(all_predictors(), model = base_model, num_comp = 2,
                target = Species, id = "importance_filter")

prepped <- prep(rec)

# create a model specification
clf <- decision_tree(mode = "classification") %>%
set_engine("rpart")

clf_fitted <- clf %>%
  fit(Species ~ ., juice(prepped))
```
