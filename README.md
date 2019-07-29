<!-- README.md is generated from README.Rmd. Please edit that file -->
**NOTE: This is a toy package created for expository purposes. It is not intended to be used for any other purpose.**

### Hacking intervals

There are two types of hacking intervals:

-   **Prescriptively constrained** hacking intervals give the range of results that could be achieved subject to user specified constraints.
-   **Tethered** hacking intervals compute range of results that could be achieved subject to *any* manipulation so long as the loss changes by only a small amount.

This package computes tethered and prescriptively constrained hacking intervals, as well as an interval that considers both types of hacking, for linear models.

For the prescriptively constrained hacking intervals, up to one of the following modifications is considered at once:

-   Removing any additive term in the base model.
-   Applying a transformation (currently `x^2` only) to any linear term included in the base model.
-   Adding a linear term for any variable in the dataset.
-   Removing a single observation.

### Installation

``` r
devtools::install_github("beauCoker/hacking")
```

### Quick demo

Start with a datset. For this demo we'll generate a toy dataset `data`.

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

So, the ordinary least squares estimate for the coefficent `beta_0` on the treatment variable `w` is about 0.27. A standard question in statistics is to ask, "what could happen if I estimated `beta_0` using a different datset drawn from the same distribution?" This is what a standard confidence interval tells you. It can be computed with `R`'s built-in `confint` function:

``` r
(ci <- confint(mdl)['w',])
#>      2.5 %     97.5 % 
#> -0.2619048  0.8011494
```

Now we get to the hacking interval part. What if instead you ask, "what if the scientist that reported this estimate threw out some important observations, or messed with the data in some other way? What's the range of estimates that could have been reported?" This is what a hacking interval tells you. For linera models, it can be computed with the `hackint_lm` function in this package:

``` r
library(hacking)
output <- hackint_lm(mdl, data, theta=0.1)
#>                       result      value          modification
#> 1             Tethered (LB): -0.1262315                  <NA>
#> 2             Tethered (UB):  0.6654762                  <NA>
#> 3          Constrained (LB):  0.1675830 Remove observation 29
#> 4          Constrained (UB):  0.3508530 Remove observation 13
#> 5 Constrained+Tethered (LB): -0.4013983 Remove observation 29
#> 6 Constrained+Tethered (UB):  0.9234986      Add variable Z.2
```

The parameter `theta` tells you what percentage of loss is tolerated for tetehred hacking. `LB` and `UB` stand for lower bound and upper bound. This means that a tethered hacking interval around the base model is (-0.13, 0.67), a prescriptively constrained hacking interval around the base model is (0.17, 0.35), and a hacking interval that considers both types is (-0.4, 0.92). Notice either of the tethered intervals are wider than the standard confidence interval.

`hackint_lm` also returns the full list of all the manipulations that were tried, the minimum-loss estimate `Estimate` for each manipulation, and the tethered hacking interval `(LB,UB)` around each. The output is sorted by the largest absolute difference `largest_diff` of any value (`LB`, `Estiamte`, or `UB`) from `beta_0`:

``` r
head(output$hacks_all)
#> # A tibble: 6 x 6
#>   type       modification              LB Estimate    UB largest_diff
#>   <chr>      <chr>                  <dbl>    <dbl> <dbl>        <dbl>
#> 1 remove_obs Remove observation 29 -0.401    0.168 0.737        0.671
#> 2 add_term   Add variable Z.2      -0.222    0.351 0.923        0.654
#> 3 remove_obs Remove observation 35 -0.373    0.192 0.757        0.643
#> 4 remove_obs Remove observation 13 -0.200    0.351 0.901        0.632
#> 5 remove_obs Remove observation 3  -0.199    0.350 0.899        0.629
#> 6 remove_obs Remove observation 30 -0.256    0.318 0.892        0.622
```
