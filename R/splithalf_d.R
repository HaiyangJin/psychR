

#' Calculate split half reliability for d'
#' @description split the data randomly into two halves and calculate the d'
#' (with [sdt()]) for the two subsets. Then calculate the Cronbach's alpha
#' corrected with Spearman-Brown method.
#'
#' @param .data *df* dataframe (long format)
#' @param SN *str* name of the column in `.data` describing whether each trial
#' showed *signal* or *noise*. Default to "SD".
#' @param SubjID *str* name of the column in `.data` describing participant IDs.
#' Default to "SubjID".
#' @param correction *boo* whether to apply correction to the reliability.
#' Default to TRUE.
#' @param startseed *int* integer for the starting seed. Default to 1215.
#' @param iter *int* number of iterations for calculating the split half
#' reliability. Default to 100.
#' @param cores *int* number of cores used to run the iterations. Default to 1.
#' @param ... other arguments pass to [sdt()].
#' * __isSignal__ _str_ name of the column in `.data` describing whether it
#' was reported *signal* on each trial. Default to "isSame".
#' * __signal__ _str_ the level name for _signal_ in `SN` of `.data`. Default
#' to "same".
#' * __d_correction__ _str_ method used to correct 1 and 0 in hits and false
#' alarm rates. There are two methods: "MK1985" (default) and "SC1988". More see
#' [sdt()].
#'
#' @return a data frame
#' * __iter__ the number of iteraction
#' * __alpha__ cronbach alpha
#' * __alpha_cor__ cronbach alpha corrected by Spearman-Brown method
#' * __seed__ seed used to split the data
#' @export
#'
#' @examples
#' splithalf_d(jin2022noncon)
splithalf_d <- function(.data, SN = "SD", SubjID = "SubjID", correction=TRUE,
                        startseed=1215, iter = 100, cores = 1, ...){

  # set parallel processing
  seeds <- startseed+1:iter
  ls_df <- pbapply::pblapply(seeds, splithalf_d_one, cl=cores,
                             .data=.data, SN=SN, SubjID=SubjID,
                             correction=correction, ...)
  df_alpha_d <- dplyr::bind_rows(ls_df, .id = "iter")

  return(df_alpha_d)
}

# only calculate the split-half reliability once
splithalf_d_one <- function(.data, SN, SubjID, correction, seed, ...){
  set.seed(seed)

  # hit and false alarm rates for each
  alpha_output <- .data |>
    dplyr::group_by_at(dplyr::vars(SubjID, SN)) |>
    dplyr::summarize(.data$isSame,
                     half = 2-(sample(dplyr::n(), dplyr::n())>(dplyr::n()/2)),
                     .groups = "keep") |>
    # dplyr::group_by_at(vars(SubjID, SN, "half")) |>
    sdt(SN = SN, SubjID = SubjID, group_other = "half", dfonly=TRUE, ...) |>
    tidyr::pivot_wider(values_from = .data$d, names_from = .data$half) |>
    dplyr::select(-c(.data$SubjID, .data$correction_d)) |>
    psych::alpha(max=100)

  # CI1 (alpha_output$feldt)

  thealpha <- alpha_output$total$raw_alpha
  if (correction) {
    # Spearman-Brown correction
    alpha_cor <- (2*thealpha)/(1+thealpha)
  } else {
    alpha_cor <- thealpha
  }

  out <- data.frame(
    alpha = thealpha,
    alpha_cor = alpha_cor,
    seed = seed
  )

  return(out)
}
