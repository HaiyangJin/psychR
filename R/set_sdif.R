
#' Apply successive difference contrast coding
#' @description
#' Apply successive difference contrast coding (\code{MASS::contr.sdif()}) to
#' variables in data and create variables to be used in steps of obtaining the
#' optimal mixed-effects models.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from \code{dbplyr} or \code{dtplyr})
#' @param varnames the columns to be applied contrasting code (without quotes)
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' set_sdif(jin2022noncon, Congruency) |> str()
#' set_sdif(jin2022noncon, c(Congruency, Alignment)) |> str()
set_sdif <- function(.data, varnames){

  varnames <- deparse(substitute(varnames)) |>
    stringr::str_remove("c\\(") |>
    stringr::str_remove("\\)") |>
    stringr::str_remove(" ") |>
    stringr::str_split(",", simplify = TRUE) |>
    as.vector()

  for (vn in varnames){

    # check number of levels
    N <- nlevels(.data[[vn]])
    message(sprintf("There are %d levels in %s.", N, vn))

    # apply contrast coding
    contrasts(.data[[vn]]) <- MASS::contr.sdif(N)

  }

  return(.data)
}
