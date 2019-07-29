#' Hacking intervals for linear models
#'
#' @param mdl model
#' @param data dataframe
#' @param theta numeric
#' @param verbose boolean
#'
#' @return list
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
hackint_lm <- function(mdl, data, theta = 0.05, verbose=TRUE) {

  formula_0 <- stats::update(stats::as.formula(mdl$call$formula), .~.)

  terms_included <- setdiff(stringr::str_trim(stringr::str_split(deparse(formula_0[[3]]), "\\+")[[1]]), 'w')

  vars_included <- setdiff(all.vars(formula_0)[-1], 'w')
  vars_all <- setdiff(colnames(data), c('y','w'))
  vars_excluded <- setdiff(vars_all, vars_included)

  terms_linear_included <- terms_included[terms_included %in% vars_all]

  n_obs = nrow(mdl$model)

  # Number of hacked models to run based on user options
  hacks_add_term = data.frame(type='add_term', var_name = vars_excluded, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Add variable ",var_name))

  hacks_remove_term = data.frame(type='remove_term', term = terms_included, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Remove term ",term))

  hacks_transformation = data.frame(type='transformation', term = terms_linear_included, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Square variable ",term))

  hacks_interaction = data.frame(type='interaction',
                                 term1 = t(utils::combn(terms_linear_included,2))[,1],
                                 term2 = t(utils::combn(terms_linear_included,2))[,2], stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = sprintf("Adding %s:%s term", term1, term2))

  hacks_obs = data.frame(type='remove_obs', row_idx = 1:n_obs, stringsAsFactors = FALSE) %>%
    dplyr::mutate(manipulation = paste0("Remove observation ",row_idx))

  # Base model fit
  SSE_0 <- sum(mdl$residuals^2)
  beta_0 = mdl$coefficients['w']

  hi_0 <- tethered_lm(mdl, theta)

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

  # Remove observation ----------
  hacks_obs <- hacks_obs %>%
    dplyr::group_by(row_idx) %>%
    dplyr::mutate(hi=list(tethered_lm(stats::update(mdl, data=data[-row_idx,]), theta) )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LB = purrr::map_dbl(hi,'LB'),
           Estimate = purrr::map_dbl(hi,'Estimate'),
           UB = purrr::map_dbl(hi,'UB')) %>%
    dplyr::select(-hi)

  # Combine results ----------
  hacks_all <- dplyr::bind_rows(hacks_add_term, hacks_remove_term, hacks_interaction, hacks_transformation, hacks_obs) %>%
    dplyr::select(type, manipulation, LB, Estimate, UB) %>%
    dplyr::mutate(largest_diff = pmax(abs(beta_0-LB), abs(UB-beta_0))) %>%
    dplyr::arrange(dplyr::desc(largest_diff)) %>%
    select(modification, everything())


  # Print results ----------

  summary <-  dplyr::bind_rows(
    data.frame(
      result = 'Tethered (LB):',
      value=hi_0['LB'],
      manipulation=NA,
      stringsAsFactors = FALSE
    ),

    data.frame(
      result = 'Tethered (UB):',
      value=hi_0['UB'],
      manipulation=NA,
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
      dplyr::select(result, value=LB, manipulation),

    hacks_all %>%
      dplyr::top_n(1, UB) %>%
      dplyr::mutate(result = 'Constrained+Tethered (UB):') %>%
      dplyr::select(result, value=UB, manipulation)

  )

  if (verbose){
    print(as.data.frame(summary))
  }

  # Format output ----------
  output <- list()
  output$tethered <- c(hi_0['LB'], hi_0['UB'])
  output$constrained <- c(min(hacks_all$Estimate), max(hacks_all$Estimate))
  output$tethered_and_constrained <- c(min(hacks_all$LB), max(hacks_all$UB))
  output$summary <- summary
  output$hacks_all <- hacks_all

  return(output)
}
