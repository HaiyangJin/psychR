#' Make random effect variables/columns
#' @description
#' Make a string/character to be used as random effect in linear mixed-effect
#' analysis.
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from \code{dbplyr} or \code{dtplyr}).
#' @param ... <tidy-select> One or more unquoted expressions separated by
#' commas. Variable names can be used as if they were positions in the data
#' frame, so expressions like x:y can be used to select a range of variables.
#' see ... in [dplyr::select()].
#'
#' @return a character/string.
#' @export
#'
#' @examples
#' mkrandom(jin2022noncon, Congruency:SD)
mkrandom <- function(.data, ...){
  recols <- .data |>
    dplyr::select(...) |>
    colnames() |>
    paste(collapse=" + ")

  return(recols)
}
