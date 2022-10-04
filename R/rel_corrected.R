
#' Calculate the reliability of subtraction or regression scores
#' @description This function calculates the reliability of subtraction
#' `(..., method="subtraction")` or regression `(..., method="regression")` scores between the two inputs.
#'
#' @param re_main reliability of the condition of main interest. (It could be
#' Guttman's lambda2 or Cronbach's alpha.)
#' @param re_base reliability of the baseline condition. (It could be
#' Guttman's lambda2 or Cronbach's alpha.)
#' @param raw_main the raw performance of the condition of main interest for all
#' participants.
#' @param raw_base the raw performance of the baseline condition for all
#' participants.
#' @param method the method to be used to correct the reliability. Default to
#' "subtraction". Options are "subtraction" or "regression".
#'
#' @return
#' A list of the relevant information:
#'
#' * reliability: a numeric value for the reliability.
#' * approach: the approach used to calculate the reliability.
#'
#' @import stats
#' @export
#'
#' @examples
#' raw_main <- c(2, 3, 2.5, 5, 3, 4, 2, 1, 3, 1)
#' raw_base <- c(2, 5, 2, 1, 3, 5, 3, 2, 4, 1)
#' rel_corrected(.9, .8, raw_main, raw_base)
#' rel_corrected(.9, .8, raw_main, raw_base, method="reg")
rel_corrected <- function(re_main, re_base, raw_main, raw_base,
                          method = "subtraction"){

  if (startsWith("subtraction", method)) {
    relvalue <- rel_subt(re_main, re_base, raw_main, raw_base)
    approach <- "subtraction"
  } else if (startsWith("regression", method)) {
    relvalue <- rel_regr(re_main, re_base, raw_main, raw_base)
    approach <- "regression"
  } else {
    stop('Unknown method for correcting reliability.')
  }

  out <- list(reliability = relvalue,
              approach = approach)

  return(out)
}

# function to calculate the reliability of subtraction scores
rel_subt <- function(re_main, re_base, raw_main, raw_base){

  tmp <- 2*sd(raw_main)*sd(raw_base)*cor(raw_main, raw_base)

  top <- var(raw_main) * re_main + var(raw_base) * re_base - tmp
  bot <- var(raw_main) + var(raw_base) - tmp

  return(top/bot)
}

# function to calculate the reliability of subtraction scores
rel_regr <- function(re_main, re_base, raw_main, raw_base){

  tmp1 <- cor(raw_main, raw_base)^2

  top <- re_main + re_base * tmp1 - 2 * tmp1
  bot <- 1 - tmp1

  return(top/bot)
}

