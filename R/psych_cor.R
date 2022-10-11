
#' Tidied up correlation outputs
#' @description Tidied up the output of `psych::corr.test()`.
#' @param .data a data frame for correlation. Each row is one participant and
#' each column is one variable.
#' @param alpha alpha level of confidence intervals. Default to .05.
#' @param ... arguments pass to `psych::corr.test()`.
#'
#' @return data frame of correlations
#' * lower, upper: lower and upper of the (uncorrected) confidence interval boundaries
#' * r: correlation coefficients
#' * p: uncorrected p-value
#' * N: sample size
#' * alpha: alpha used to calculated the uncorrected CIs
#' * p.adj: corrected (adjusted) p-value
#' * adjust: the adjust method
#' * alt: two-sided tests
#' * lower.adj, upper.adj: lower and upper of the corrected confidence interval boundaries
#' @export
#'
#' @examples
#' psych_cor(attitude)
psych_cor <- function(.data, alpha=.05, ...) {

  # run correlations
  cor_tmp <- psych::corr.test(.data, alpha=alpha, ...)

  out <- cor_tmp$ci |>
    dplyr::rename(`lower`=.data$lower,
                  `upper`=.data$upper) |>
    dplyr::mutate(N = cor_tmp$n,
                  alpha = alpha,
                  p.adj = cor_tmp$p.adj,
                  adjust = cor_tmp$adjust,
                  alt = "two.sided") |>
    dplyr::bind_cols(cor_tmp$ci.adj |>
                       dplyr::mutate(r=1) |>
                       dplyr::select(-.data$r))

  return(out)
}
