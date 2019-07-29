<!-- README.md is generated from README.Rmd. Please edit that file -->
**NOTE: This is a toy package created for expository purposes. The code is not intended to be used for any purpose**

### Hacking intervals

\[Stuff about hacking intervals, to be improved\]

There two types of hacking intervals:

-   **Prescriptively constrained** hacking intervals give the range of results that could be achieved subject to user specified constraints.
-   **Tethered** hacking intervals compute range of results that could be achieved subject to *any* manipulation so long as the loss changes by only a small amount.

This package computes tethered and prescriptively constrained hacking intervals for linear models. Up to one of the following prescriptive constraints is considered at a time:

-   Removing any additive term in the base model.
-   Applying a transformation (currently `x^2` only) to any linear term included in the base model.
-   Adding a linear term for any variable in the dataset.
-   Removing a single observation.

### Installation

``` r
devtools::install_github("beauCoker/hacking")
```

### Quick demo

Sample data:

``` r
N = 10
Px = 3
Pz = 3

set.seed(1)
data <- data.frame(
  y = rnorm(N), # Response variable (continuous)
  w = rbinom(N, 1, .5), # Treatment variable (binary)
  X = matrix(rnorm(N*Px), nrow=N), # Covariates included in base model
  Z = matrix(rnorm(N*Pz), nrow=N) # Covaraites excluded from base model
)
```

Fit a linear base model with `lm` and examine the coefficient on the treatment variable:

``` r
mdl <- lm(y ~ w + X.1*X.2, data=data)
(beta_0 <- mdl$coefficients['w'])
#>         w 
#> -1.427219
```

The OLS estimate for the coefficent `beta_0` on the treatment variable `w` is -1.4272186. Compute the hacking interval around this value with `hacking_lm`:

``` r
library(hacking)
output <- hackint_lm(mdl, data, theta=0.5)
#>                       result      value         modification
#> 1             Tethered (LB): -1.7418536                 <NA>
#> 2             Tethered (UB): -1.1125835                 <NA>
#> 3          Constrained (LB): -1.8613618 Remove observation 8
#> 4          Constrained (UB): -0.6719029      Remove term X.1
#> 5 Constrained+Tethered (LB): -2.9554841     Add variable Z.2
#> 6 Constrained+Tethered (UB):  0.4130532 Remove observation 3
```

Full output, sorted by the largest difference from `beta_0`:

``` r
head(output$hacks_all)
#> # A tibble: 6 x 6
#>   type        modification            LB Estimate      UB largest_diff
#>   <chr>       <chr>                <dbl>    <dbl>   <dbl>        <dbl>
#> 1 remove_obs  Remove observation 3 -2.12   -0.851  0.413          1.84
#> 2 remove_term Remove term X.1      -1.75   -0.672  0.410          1.84
#> 3 add_term    Add variable Z.2     -2.96   -1.78  -0.606          1.53
#> 4 remove_obs  Remove observation 8 -2.93   -1.86  -0.793          1.50
#> 5 remove_obs  Remove observation 1 -2.79   -1.54  -0.282          1.36
#> 6 remove_term Remove term X.1:X.2  -2.34   -1.21  -0.0829         1.34
```
