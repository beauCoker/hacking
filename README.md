<!-- README.md is generated from README.Rmd. Please edit that file -->
**NOTE: This is a toy package created for expository purposes. The code is not intended to be used for any purpose**

### Hacking intervals

Stuff about hacking intervals

### Installation

``` r
devtools::install_github("beauCoker/hacking")
```

### Quick demo

Create some data:

``` r
N = 10
Px = 3
Pz = 3

set.seed(1)
y = rnorm(N)
w = rbinom(N, 1, .5)
X = matrix(rnorm(N*Px), nrow=N)
Z = matrix(rnorm(N*Pz), nrow=N)

data <- data.frame(y=y,w=w,X=X, Z=Z)
```

Fit a linear model with `lm`:

``` r
mdl <- lm(y ~ w + X.1*X.2, data=data)
```

Compute the hacking interval with `hacking_lm`:

``` r
library(hacking)
out <- hackint_lm(mdl,data)
#>                       result      value         modification
#> 1             Tethered (LB): -1.7418536                 <NA>
#> 2             Tethered (UB): -1.1125835                 <NA>
#> 3          Constrained (LB): -1.8613618 Remove observation 8
#> 4          Constrained (UB): -0.6719029      Remove term X.1
#> 5 Constrained+Tethered (LB): -2.1990505 Remove observation 8
#> 6 Constrained+Tethered (UB): -0.3299262      Remove term X.1
```
