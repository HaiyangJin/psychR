
#' Calculate reliability for long-format data frame
#' @description **This function are not developed very well! Please use with caution.**
#' Calculate reliability of data frame in long-format with [psych::splitHalf()].
#'
#' @param .data *df* data frame (long format)
#' @param DV *str* name of the column for the dependent variable.
#' @param cor_method *function handle* the correlation methods to be used. Options
#' are `cor` (default), `cov`, and `pol` (for [psych::polychoric()]). When using
#' `cov`, you may also want to specify `covar=TRUE`.
#' @param item *str* name of the column for testing item (e.g., stimulus, survey
#'  questions). Default to "test_item".
#' @param fake_tn *boo* whether to add fake trial indices. Default to False.
#' @param SubjID *str* name of the column for subject IDs. Default to "SubjID".
#' @param ... other arguments pass to [psych::splitHalf()].
#'
#' @return object from [psych::splitHalf()].
#' @import psych
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' jin2022noncon |>
#'   dplyr::filter(Congruency=="con", Alignment=="ali") |>
#'   rel_each(DV="RT", fake_tn=TRUE)
#'
rel_each <- function(.data, DV,
                     cor_method=cor,
                     item="test_item", fake_tn=FALSE, SubjID="SubjID", ...){

  if (fake_tn){
    # add fake trial index

    if (item %in% colnames(.data)){
      warning(sprintf('Column (%s) will be replaced by fake trial indices', item))
    }

    .data <- .data |>
      dplyr::group_by_at(dplyr::vars(SubjID)) |>
      dplyr::mutate(!!item := paste0("t", 1:dplyr::n())) |>
      dplyr::ungroup()

  } else if (!item %in% colnames(.data)) {
    stop(sprintf('Cannot find the column (%s) in .data.', item))
  }

  # cor, cov, pol(ychoric)
  relout <- .data |>
    tidyr::pivot_wider(all_of(SubjID), values_from = all_of(DV), names_from = all_of(item)) |>
    dplyr::select(-all_of(SubjID)) |>
    data.matrix() |>
    cor_method() |> # create the correlation matrix (different methods)
    splitHalf(check.keys=FALSE, ...)

  return(relout)
}

pol <- function(thematrix){
  # wrapper to render the output to be the correlation matrix
  tmpout <- psych::polychoric(thematrix)
  return(tmpout$rho)
}
