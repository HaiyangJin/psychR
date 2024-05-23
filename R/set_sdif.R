
#' Apply successive difference contrast coding
#' @description
#' Apply successive difference contrast coding (\code{MASS::contr.sdif()}) to
#' variables in data and create variables to be used in steps of obtaining the
#' optimal mixed-effects models.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from \code{dbplyr} or \code{dtplyr})
#' @param colname the column to be applied contrasting code (without quotes)
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' set_sdif(jin2022noncon, Congruency)
set_sdif <- function(.data, colname){

  # convert column name to string if needed
  colname <- deparse(substitute(colname))

  # check number of levels
  N <- nlevels(.data[[colname]])
  message(sprintf("There are %d levels in %s.", N, colname))

  # apply contrast coding
  contrasts(.data[[colname]]) <- MASS::contr.sdif(N)

  return(.data)
}
