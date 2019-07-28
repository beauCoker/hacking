#' Tethered hacking interval for linear model
#'
#' @param mdl model
#' @param theta numeric
#'
#' @return vector
#' @export
#'
#' @examples
tethered_lm <- function(mdl, theta=0.05){
  SSE <- sum(mdl$residuals^2)

  t = sqrt((((1+theta)*SSE)/SSE-1)*mdl$df.residual)
  alpha = 2*(1-stats::pt(t, mdl$df.residual))

  hi <- stats::confint(mdl, level = 1 - alpha)

  out <- c(hi['w',1],mdl$coefficients['w'],hi['w',2])
  names(out) <- c("LB","Estimate","UB")
  return(out)
}
