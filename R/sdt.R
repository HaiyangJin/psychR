
#' Calculate d' in Signal Detection Theory
#'
#' @param .data *df* dataframe (long format)
#' @param SN *str* name of the column in `.data` describing whether each trial
#' showed *signal* or *noise*. Default to "SD".
#' @param isSignal *str* name of the column in `.data` describing whether it
#' was reported *signal* on each trial. Default to "isSame".
#' @param SubjID *str* name of the column in `.data` describing participant IDs.
#' Default to "SubjID".
#' @param group_other *vector of str* a list of columns in `.data` to be grouped
#' by before calculating d'.
#' @param signal *str* the level name for *signal* in `SN` of `.data`. Default
#' to "same".
#' @param d_correction *str* method used to correct 1 and 0 in hits and false
#' alarm rates. There are two methods:
#' * "MK1985" (default): correct 1 and 0 to `(2N-1)/(2N)` and `1/(2N)` where `N` is the number of trials in each condition.
#' * "SC1988": add 0.5 to number of hits and false alarm and add 1 to the number of trials in each condition.
#' @param dfonly *boo* whether output the dataframe only. Default to `FALSE`.
#'
#' @return
#' a list
#' * __df__ a dataframe containing d
#' * __correction__ the method used to correct 1 and 0.
#' @references
#' Macmillan, N. A., & Kaplan, H. L. (1985). Detection theory analysis of group data: Estimating sensitivity from average hit and false-alarm rates. Psychological Bulletin, 98, 185–199. https://doi.org/10.1037/0033-2909.98.1.185
#'
#' Snodgrass, J. G., & Corwin, J. (1988). Pragmatics of measuring recognition memory: Applications to dementia and amnesia. Journal of Experimental Psychology: General, 117(1), 34–50. https://doi.org/10.1037/0096-3445.117.1.34
#' @import tidyselect
#' @export
#'
#' @examples
#' sdt(jin2022noncon)
#' sdt(jin2022noncon, d_correction="SC1988")
#' sdt(jin2022noncon, group_other=c("Congruency", "Alignment"))
#' sdt(jin2022noncon, dfonly=T)
sdt <- function(.data, SN = "SD", isSignal = "isSame", SubjID = "SubjID",
                group_other = NULL,
                signal = "same", d_correction="MK1985", dfonly=FALSE){

  # prepare data
  rates <- .data |>
    dplyr::select(all_of(SubjID), all_of(group_other), all_of(SN), all_of(isSignal)) |>
    dplyr::group_by_at(dplyr::vars(SubjID, group_other, SN)) |>
    dplyr::summarize(saysignal = sum(!!rlang::sym(isSignal)),
                     count = dplyr::n(),
                     .groups = "drop") |>
    dplyr::mutate(SD = factor(SD),
                  SD = forcats::fct_relevel(SD, signal),
                  SD = as.integer(SD)) # 1 will be signal and 2 will be noise

  # correct 0 and 1 in hits and false alarm
  if (startsWith("MK1985", d_correction)){
    rates_cor <- rates |>
      # replace all 1 as (2N-1)/(2N); replace all 0 as (1/(2N))
      dplyr::mutate(saysignal = .data$saysignal/.data$count,
                    saysignal = ifelse(.data$saysignal==1, (2*.data$count-1)/(2*.data$count),
                                       ifelse(.data$saysignal==0, 1/(2*.data$count),
                                              .data$saysignal)))

  } else if (startsWith("SC1988", d_correction)){
    rates_cor <- rates |>
      # add 0.5 to saysignal and 1 to total N
      dplyr::mutate(saysignal = (.data$saysignal+0.5)/(.data$count+1))

  } else {
    stop(sprintf("Cannot identify the correction methods (%s) for calcualting d'.", d_correction))
  }

  # calculate d
  df_d <- rates_cor |>
    dplyr::select(-.data$count) |>
    dplyr::mutate(saysignal = qnorm(.data$saysignal)) |>
    tidyr::pivot_wider(names_from = SD, values_from = .data$saysignal) |>
    dplyr::mutate(d = .data$`1` - .data$`2`) |>
    dplyr::select(-c(.data$`1`, .data$`2`))

  if (dfonly) {
    out <- df_d |>
      dplyr::mutate(correction_d = d_correction)

  } else {
    out <- list()
    # record the correction method
    out$correction = d_correction
    # save the df_d
    out$df <- df_d

  }

  return(out)
}
