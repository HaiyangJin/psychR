
#' Wrapper to obtain CIs from correlation coefficient.
#'
#' @param r A correlation coefficient (more see [psych::corr.test()])
#' @param n Number of observations (more see [psych::corr.test()])
#'
#' @return the CI
#' @import psych
#' @export
#'
#' @examples
#' r2ci(0.5, 50)
r2ci <- function(r, n){
  cormatrix <- matrix(c(r), nrow=1)
  rownames(cormatrix) <- c("this")
  colnames(cormatrix) <- c("this")

  out <- psych::corr.p(cormatrix, n, "none")

  return(out$ci)
}
