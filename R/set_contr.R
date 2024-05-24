
#' Apply successive difference contrast coding
#' @description
#' Apply successive difference contrast coding ([MASS::contr.sdif()]) to
#' variables in data and create variables to be used in steps of obtaining the
#' optimal mixed-effects models.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from \code{dbplyr} or \code{dtplyr}).
#' @param varnames the columns to be applied contrasting code (without quotes).
#' @param contraFUN the contrast coding function (name) to be used.
#' Default to \code{MASS::contr.sdif}. Currently popular contrast
#' coding functions are: [MASS::contr.sdif()], [contr.sum()],
#' [contr.treatment()], [contr.SAS()], etc.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' set_contr(jin2022noncon, Congruency) |> str()
#'
#' set_contr(jin2022noncon, c(Congruency, Alignment)) |> str()
#'
#' set_contr(jin2022noncon, c(Congruency, Alignment), contr.sum) |> str()
#'
set_contr <- function(.data,
                     varnames,
                     contraFUN=MASS::contr.sdif){

  varnames <- deparse(substitute(varnames)) |>
    stringr::str_remove("c\\(") |>
    stringr::str_remove("\\)") |>
    stringr::str_split(",", simplify = TRUE) |>
    as.vector()

  for (vn in varnames){

    # remove space at the beginning and end
    vn <- vn |>
      stringr::str_remove("^ ") |>
      stringr::str_remove(" $") |>
      stringr::str_remove_all("`")

    # check number of levels
    N <- nlevels(.data[[vn]])
    message(sprintf("There are %d levels in %s.", N, vn))

    # apply contrast coding
    contrasts(.data[[vn]]) <- contraFUN(N)

  }

  return(.data)
}
