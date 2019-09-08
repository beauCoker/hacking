#' Hacking intervals for linear models
#'
#' Computes tethered and constraint-based hacking intervals of the coefficient of a binary treatment variable in a linear model.
#' See \url{https://github.com/beauCoker/hacking} for examples.
#'
#' @param mdl \code{lm} object representing the "base" model
#' @param data \code{data.frame} used to fit \code{mdl}
#' @param theta loss tolerance for tethered hacking (default = 0.1)
#' @param treatment name of binary treatment variable (inputted as \code{character})
#' @param frac_remove_obs fraction of observations to consider for removal (default = 1, meaning all observations considered)
#' @param verbose whether or not to print summary
#'
#' @return \code{list} containing all hacking intervals (\code{tethered}, \code{constrained}, \code{tethered_and_constrained})
#' as well as complete list of all manipulations applied to the base model (\code{hacks_all})
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' N = 50 # Number of observations
#' data <- data.frame(
#'   y = rnorm(N), # Response variable (continuous)
#'   w = rbinom(N, 1, .5), # Treatment variable (binary)
#'   X = matrix(rnorm(N*3), nrow=N), # Covariates included in base model
#'   Z = matrix(rnorm(N*3), nrow=N) # Covariates excluded from base model
#' )
#' mdl <- lm(y ~ w + X.1*X.2, data=data) # fit linear "base" model
#' output <- hackint_lm(mdl, data, theta=0.1, treatment = 'w')
hackint_lm <- function(mdl, data, treatment, theta = 0.1, frac_remove_obs = 1, verbose=TRUE) {

  ## validate inputs
  if (class(mdl) != "lm") stop("'mdl' must be of class 'lm'")
  if (theta <= 0) stop("'theta' must be > 0")
  if (frac_remove_obs < 0 || frac_remove_obs>1) stop("'frac_remove_obs' must be between 0 and 1")
  if (any(unique(data[treatment]) != c(1,0))) stop("treatment variable must have values 1 and 0 only")

  ## base model data
  SSE_0 <- sum(mdl$residuals^2)
  beta_0 = mdl$coefficients[treatment]

  hi_0 <- tethered_lm(mdl, theta)

  ## parse inputs
  formula_0 <- stats::update(mdl$terms, .~., evaluate=FALSE)

  response <- as.character(formula_0[[2]])

  terms_included <- setdiff(attr(mdl$terms, 'term.labels'), treatment)

  vars_included <- setdiff(all.vars(formula_0)[-1], treatment)
  vars_all <- setdiff(colnames(data), c(response,treatment))
  vars_excluded <- setdiff(vars_all, vars_included)

  terms_linear_included <- terms_included[terms_included %in% vars_all]

  dataClasses = attr(mdl$terms,"dataClasses")
  vars_numeric = names(dataClasses)[dataClasses=='numeric']

  terms_linear_numeric_included = intersect(terms_linear_included, vars_numeric)

  n_obs = nrow(mdl$model)

  if (frac_remove_obs == 1){
    remove_obs_idx <- 1:n_obs
  } else {
    remove_obs_idx <- order(stats::cooks.distance(mdl), decreasing = TRUE)[1:round(frac_remove_obs*n_obs)]
  }

  ## Number of hacked models to run based on user options
  hacks_add_term = data.frame(type='add_term', var_name = vars_excluded, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Add variable ",var_name))

  hacks_remove_term = data.frame(type='remove_term', term = terms_included, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Remove term ",term))

  hacks_transformation = data.frame(type='transformation', term = terms_linear_numeric_included, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Square variable ",term))

  hacks_discretize = data.frame(type='discretize', term = terms_linear_numeric_included, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Discretize variable ",term))

  hacks_interaction = data.frame(type='interaction',
                                 term1 = t(utils::combn(terms_linear_included,2))[,1],
                                 term2 = t(utils::combn(terms_linear_included,2))[,2], stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = sprintf("Adding %s:%s term", term1, term2))

  if (frac_remove_obs > 0) {
    hacks_obs = data.frame(type='remove_obs', row_idx = remove_obs_idx, stringsAsFactors = FALSE) %>%
      dplyr::mutate(manipulation = paste0("Remove observation ",row_idx))
  } else {
    hacks_obs <- data.frame(type=NA, row_idx=NA, manipulation=NA, LB=NA, Estimate=NA, UB=NA)
    hacks_obs <- hacks_obs[0,]
  }

  ## refit model under all manipulations, one type at a time

  # Add term ----------
  hacks_add_term <- hacks_add_term %>%
    dplyr::group_by(var_name) %>%
    dplyr::mutate(f = list(stats::update(formula_0, sprintf('.~. +%s',var_name)))) %>%
    dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, formula=f[[1]]), theta) )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
                  Estimate = purrr::map_dbl(hi,'Estimate'),
                  UB = purrr::map_dbl(hi,'UB')) %>%
    dplyr::select(-hi)

  # Remove term  ----------
  hacks_remove_term <- hacks_remove_term %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(f = list(stats::update(formula_0, sprintf('.~. -%s',term)))) %>%
    dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, formula=f[[1]]), theta) )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
                  Estimate = purrr::map_dbl(hi,'Estimate'),
                  UB = purrr::map_dbl(hi,'UB')) %>%
    dplyr::select(-hi)

  # Interaction term ----------
  hacks_interaction <- hacks_interaction %>%
    dplyr::group_by(term1, term2) %>%
    dplyr::mutate(f = list(stats::update(formula_0, sprintf('.~. +%s:%s',term1,term2)))) %>%
    dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, formula=f[[1]]), theta) )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
                  Estimate = purrr::map_dbl(hi,'Estimate'),
                  UB = purrr::map_dbl(hi,'UB')) %>%
    dplyr::select(-hi)

  # Transformation ----------
  hacks_transformation <- hacks_transformation %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(f = list(stats::update(formula_0, sprintf('.~. + I(%s^2)',term)))) %>%
    dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, formula=f[[1]]), theta) )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
                  Estimate = purrr::map_dbl(hi,'Estimate'),
                  UB = purrr::map_dbl(hi,'UB')) %>%
    dplyr::select(-hi)

  # Discretize ----------
  hacks_discretize = hacks_discretize %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(f = list(stats::update(formula_0, '.~. + discretized_term'))) %>%
    dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, formula=f[[1]], data=cbind(data, discretized_term = discretize(data[[term]])))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
                  Estimate = purrr::map_dbl(hi,'Estimate'),
                  UB = purrr::map_dbl(hi,'UB')) %>%
    dplyr::select(-hi)

  # Remove observation ----------
  if (frac_remove_obs > 0) {
    hacks_obs <- hacks_obs %>%
      dplyr::group_by(row_idx) %>%
      dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, data=data[-row_idx,]), theta) )) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
                    Estimate = purrr::map_dbl(hi,'Estimate'),
                    UB = purrr::map_dbl(hi,'UB')) %>%
      dplyr::select(-hi)
  }

  # Combine results ----------
  hacks_all <- dplyr::bind_rows(hacks_add_term, hacks_remove_term, hacks_interaction,
                                hacks_transformation, hacks_obs, hacks_discretize) %>%
    dplyr::select(type, manipulation, LB, Estimate, UB) %>%
    dplyr::mutate(largest_diff = pmax(abs(beta_0-LB), abs(UB-beta_0))) %>%
    dplyr::arrange(dplyr::desc(largest_diff)) %>%
    dplyr::select(manipulation, dplyr::everything())


  ## Format output
  summary <-  dplyr::bind_rows(
    data.frame(
      result = 'Tethered (LB):',
      value=hi_0['LB'],
      manipulation='Tethered',
      stringsAsFactors = FALSE
    ),

    data.frame(
      result = 'Tethered (UB):',
      value=hi_0['UB'],
      manipulation='Tethered',
      stringsAsFactors = FALSE
    ),

    hacks_all %>%
      dplyr::top_n(-1, Estimate) %>%
      dplyr::mutate(result = 'Constrained (LB):') %>%
      dplyr::select(result, value=Estimate, manipulation),

    hacks_all %>%
      dplyr::top_n(1, Estimate) %>%
      dplyr::mutate(result = 'Constrained (UB):') %>%
      dplyr::select(result, value=Estimate, manipulation),

    hacks_all %>%
      dplyr::top_n(-1, LB) %>%
      dplyr::mutate(result = 'Constrained+Tethered (LB):') %>%
      dplyr::select(result, value=LB, manipulation) %>%
      dplyr::mutate(manipulation = paste(manipulation,'+ Tethered')),

    hacks_all %>%
      dplyr::top_n(1, UB) %>%
      dplyr::mutate(result = 'Constrained+Tethered (UB):') %>%
      dplyr::select(result, value=UB, manipulation) %>%
      dplyr::mutate(manipulation = paste(manipulation,'+ Tethered'))

  )

  if (verbose){
    print(as.data.frame(summary))
  }

  output <- list()
  output$tethered <- c(hi_0['LB'], hi_0['UB'])
  output$constrained <- c(min(hacks_all$Estimate), max(hacks_all$Estimate))
  output$tethered_and_constrained <- c(min(hacks_all$LB), max(hacks_all$UB))
  output$summary <- summary
  output$hacks_all <- hacks_all

  return(output)
}
