#' Add new columns/varaibles for LMM analysis
#' @description
#' Based on contrast coding of each independent variable to add new
#' columns/variables to facilitate linear mixed-effects analysis.
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from \code{dbplyr} or \code{dtplyr}). See \code{data} in
#' [model.matrix].
#' @param object an object of an appropriate class. For the default method, a
#' model [formula] or a [terms] object. See \code{object} in [model.matrix].
#' @param Nstr the number (integer) of strings to be kept in the name.
#' Default to 3.
#'
#' @return A data frame with new columns/variables.
#' @export
#'
#' @examples
#' add_lmmcols(jin2022noncon, isSame ~ Congruency * Alignment + SD) |> str()
#'
add_lmmcols <- function(.data, object, Nstr=3){

  # make data frame for linear mixed-effects model analysis
  df_lmm <- model.matrix(data = .data, object) |>
    as.data.frame() |>
    dplyr::select(-"(Intercept)")

  # get the original column names of `df_lmm`
  lmmcols <- colnames(df_lmm)
  lmmcols_new <- lmmcols # make a copy

  # column names of the input data frame .data
  dfcols <- colnames(.data)
  dfcolslmm <- dfcols[sapply(dfcols, \(x) any(startsWith(lmmcols_new, x)))]
  dfcolslmm <- names(sort(sapply(dfcolslmm, \(x) which(startsWith(lmmcols_new, x))[1])))
  dfcols_short <- sapply(dfcolslmm, \(x) substr(x,1,Nstr))

  # update lmmcols_new with shorter names
  for (vn in 1:length(dfcols_short)){
    lmmcols_new <- stringr::str_replace(lmmcols_new, lmmcols_new[vn], dfcols_short[[vn]])
  }

  # add "_C" to non-interaction terms OR replace ":" with "_"
  lmmcols_new <- sapply(lmmcols_new, \(x){
    if (!stringr::str_detect(x, ":")){
      x = paste0(x, "_C")
    } else {
      x = stringr::str_replace(x, ":", "_")
    }
  })

  # rename columns of df_lmm
  names(lmmcols) <- lmmcols_new
  df_lmm <- dplyr::rename(df_lmm, all_of(lmmcols))

  # output the combined data frame
  return(cbind(.data, df_lmm))
}
