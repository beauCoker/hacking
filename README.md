<!-- README.md is generated from README.Rmd. Please edit that file -->
**NOTE: This is a toy package created for expository purposes. It is not intended to be used for any other purpose.**

### Hacking intervals

"Could a different reasonable researcher analyzing the same data come to a different conclusion?" This is a question that gets to the heart of whether or not a scientific result can be trusted, yet it's a question that traditional statistical inference has little to say about.

We introduce the *hacking interval*, which is the range of a numerical scientific result that could be obtained over a set of reasonable dataset and hyperparameter manipulations. Hacking intervals come in two varieties:

-   **Prescriptively-constrained** hacking intervals find the range of results over a user-defined set of reasonable manipulations. This provides explicit robustness to manipulations like:
    -   Removing an outlier.
    -   Adding/removing a feature.
    -   Adding an interaction term.
    -   Adding a transformation of a feature (like `x^2`).
-   **Tethered** hacking intervals find the range of results over the set of models that fit the data well compared to a "base" model, supposing that each such model could be obtained by an unidentified manipulation. This provides robustness to *any* manipulation, including ones difficult for optimization like:
    -   Changing the values of covariates or outcomes.
    -   Removing any number of observations.
    -   Adding new observations or new features.
    -   Changing hyperparameters.

This package computes tethered and prescriptively-constrained hacking intervals for linear models. We also compute an interval that considers both types of hacking at once; in other words, it is the range result that could be achieved by a model that fits the data almost as well as any model obtainable via the prescriptive constraints. At most one of the manipulations in the prescriptive constraints is permitted at a time.

### Installation

``` r
devtools::install_github("beauCoker/hacking")
```

### Quick demo

Start with a dataset. For this demo we'll generate a toy dataset `data`.

``` r
set.seed(0)

N = 50 # Number of observations
data <- data.frame(
  y = rnorm(N), # Response variable (continuous)
  w = rbinom(N, 1, .5), # Treatment variable (binary)
  X = matrix(rnorm(N*3), nrow=N), # Covariates included in base model
  Z = matrix(rnorm(N*3), nrow=N) # Covariates excluded from base model
)
```

Next, fit a linear model with `lm`. We'll call this the "base" model.

``` r
mdl <- lm(y ~ w + X.1*X.2, data=data)
(beta_0 <- mdl$coefficients['w'])
#>         w 
#> 0.2696223
```

So, the ordinary least squares estimate for the coefficient `beta_0` on the treatment variable `w` in the base model is about 0.26. A standard question in statistics is to ask, "what could happen if I estimated `beta_0` using a different dataset drawn from the same distribution?" This is conceptually what a standard confidence interval tells you. It can be computed with `R`'s built-in `confint` function:

``` r
(ci <- confint(mdl)['w',])
#>      2.5 %     97.5 % 
#> -0.2619048  0.8011494
```

Now we get to the hacking interval part. What if instead you ask, "what if the scientist that reported this estimate threw out some important observations, or messed with the data in some other way? What's the range of estimates that could have been reported?" This is conceptually what a hacking interval tells you. For linear models, it can be computed with the `hackint_lm` function in our package. The parameter `theta` tells you what percentage of loss is tolerated for the tethered variety of hacking.

``` r
library(hacking)
output <- hackint_lm(mdl, data, theta=0.1)
#>                       result      value                     manipulation
#> 1             Tethered (LB): -0.2901996                         Tethered
#> 2             Tethered (UB):  0.8294442                         Tethered
#> 3          Constrained (LB):  0.1675830            Remove observation 29
#> 4          Constrained (UB):  0.3508530            Remove observation 13
#> 5 Constrained+Tethered (LB): -0.4013983 Remove observation 29 + Tethered
#> 6 Constrained+Tethered (UB):  0.9234986      Add variable Z.2 + Tethered
```

In the output above, `LB` and `UB` stand for lower bound and upper bound. It says that a tethered hacking interval around the base model is (-0.29, 0.82), a prescriptively constrained hacking interval around the base model is (0.16, 0.35), and a hacking interval that considers both types hacking is (-0.4, 0.92).

Notice either of the tethered intervals are wider than the standard confidence interval, (-0.26, 0.8).

`hackint_lm` also returns the full list of all the manipulations that were tried, the minimum-loss estimate `Estimate` for each manipulation, and the tethered hacking interval `(LB,UB)` around each. The output is sorted by the largest absolute difference `largest_diff` of any value (`LB`, `Estimate`, or `UB`) from `beta_0`:

``` r
head(output$hacks_all)
#> # A tibble: 6 x 6
#>   manipulation          type           LB Estimate    UB largest_diff
#>   <chr>                 <chr>       <dbl>    <dbl> <dbl>        <dbl>
#> 1 Remove observation 29 remove_obs -0.401    0.168 0.737        0.671
#> 2 Add variable Z.2      add_term   -0.222    0.351 0.923        0.654
#> 3 Remove observation 35 remove_obs -0.373    0.192 0.757        0.643
#> 4 Remove observation 13 remove_obs -0.200    0.351 0.901        0.632
#> 5 Remove observation 3  remove_obs -0.199    0.350 0.899        0.629
#> 6 Remove observation 30 remove_obs -0.256    0.318 0.892        0.622
```

### Other functionality

#### Focusing on most influential observations

The optional argument `frac_remove_obs` (default value 1) specifies the fraction of observations that are considered for removal in evaluating prescriptively-constrained hacking intervals. If `frac_remove_obs` is less than 1, then only observations with the highest Cook's distance are considered for removal. This will speed up computation for small datasets but does not provide any theoretical guarantees of accuracy.
