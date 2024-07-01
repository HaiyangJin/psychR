#' A wrapper function to save the [lmer()] and [glmer()] fit/output as a local
#' file, which will be reloaded when the code is re-run.
#'
#' @param ... arguments in [lmer()]
#' @param file file name of the local file to be saved. Default to "none", where
#' no files will be saved/loaded.
#'
#' @return the fit/output.
#' @export
#'
#' @examples
#' fit <- lmerf(log(RT) ~ Congruency * Alignment + (1|SubjID), jin2022noncon, file = "none")
lmerf <- function(..., file = "none") {

  # load file if exists
  if (!identical(file, "none") & file.exists(file)) return(readRDS(file))

  # fit the model
  fit <- lme4::lmer(...)
  # save the output
  if (!identical(file, "none")) saveRDS(fit, file = file)
  return(fit)
}

glmerf <- function(..., file = "none") {

  # load file if exists
  if (!identical(file, "none") & file.exists(file)) return(readRDS(file))

  # fit the model
  fit <- lme4::glmer(...)
  # save the output
  if (!identical(file, "none")) saveRDS(fit, file = file)
  return(fit)
}
