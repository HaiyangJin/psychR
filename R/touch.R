#' Make empty files (touch)
#' @description
#' Make empty files for uploading online.
#'
#' @param path a character vector of full path names; the default corresponds
#' to the working directory, [getwd()]. Tilde expansion (see [path.expand]) is
#' performed. Missing values will be ignored. Elements with a marked encoding
#' will be converted to the native encoding (and if that fails, considered
#' non-existent). See [list.files()].
#' @param pattern an optional [regular expression](regex). Only file names which
#' match the regular expression will be returned.
#'
#' @return NA
#' @export
#'
#' @examples
#' # touch("path_to_a_directory")
touch <- function(path, pattern = NULL){

  # sanity check to make sure the path exists
  stopifnot(dir.exists(path))

  # identify all matching files
  filelist <- list.files(path, pattern = pattern)
  message(sprintf("%d files are found and to be touched.", length(filelist)))

  # make the touch directory
  touchpath <- paste0(path, "_touch")
  dir.create(touchpath)

  # make the touch files
  touchlist <- file.path(touchpath, filelist)
  tmp <- file.create(touchlist)
}
