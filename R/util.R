#' Discretize a continuous variable
#'
#' Converts a continuous variable into a factor with levels based on quantiles of the continuous variable
#'
#' @param x numeric vector to convert
#' @param probs numeric vector of quantiles that determine factor levels
#'
#' @return factor with levels base on quantiles
#' @export
#'
#' @examples
#' discretize(rnorm(20))
discretize <- function(x, probs=c(.25,.5,.75,1)){
  breaks = c(min(x)-1e-10, stats::quantile(x, probs=probs))
  cut(x, breaks=breaks)
}
