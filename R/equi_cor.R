
#' Perform Equivalence Tests for correlations
#' @description Perform equivalence tests with `TOSTER::TOSTr()`, where the
#' correlation coefficient is obtained from `psych::corr.test()`.
#'
#' @param .data a data frame for correlation. Each row is one participant and
#' each column is one variable.
#' @param low_eqbound_r the lower boundary to be tested against. Default to -1.
#' @param high_eqbound_r the upper boundary to be tested against. Default to 1.
#' @param alpha alpha level of confidence intervals. Default to .10.
#' @param ... argument pass to `TOSTER::TOSTr()`.
#'
#' @return a data frame:
#' * r the correlation coefficient
#' * tost_pmax the maximum p-value of the two one-sided tests.
#' * tost_lw, tost_up the boundaries of the confidence intervals for the TOST.
#' * alpha alpha level of confidence intervals.
#' * low_eqboundar_r, high_eqbound_r the applied boundaries to be tested against with.
#' @export
#'
#' @examples
#' equi_cor(attitude, -.25, .25)
equi_cor <- function(.data, low_eqbound_r=-1, high_eqbound_r=1, alpha=.10, ...){

  corr_mat <- psych_cor(.data, alpha=alpha/2)

  r <- c()
  tost_pmax <- c()
  tost_lw <- c()
  tost_hi <- c()

  for (irow in 1:nrow(corr_mat)) {

    thisr <- corr_mat[irow,]

    tmp <- TOSTER::TOSTr(thisr$N, thisr$r, low_eqbound_r, high_eqbound_r,
                         plot = FALSE, verbose = FALSE, alpha=alpha/2, ...)

    r[irow] <- tmp$r
    tost_pmax[irow] <- max(tmp$TOST_p1, tmp$TOST_p2)
    tost_lw[irow] <- tmp$LL_CI_TOST
    tost_hi[irow] <- tmp$UL_CI_TOST
  }

  equir_out <- data.frame(r = r,
                          tost_pmax = tost_pmax,
                          tost_lw = tost_lw,
                          tost_up = tost_hi,
                          alpha = alpha,
                          low_eqbound_r = low_eqbound_r,
                          high_eqbound_r = high_eqbound_r)

  rownames(equir_out) <- rownames(corr_mat)

  return(equir_out)
}
