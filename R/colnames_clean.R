#' Clean column names
#'
#' @param dta output data of the preceding function ie., filename_separate function. The colnames_clean function assumes its input data is already loaded in the environment.
#' if input data is not already available in the environment, ensure that it is loaded or read it into the environment.
#'
#' @return a tibble of the input data but with cleaned column names eg. lower case column names, removal of special characters in column names
#' @export
#'

colnames_clean <- function(dta){

  colnames(dta) <- tolower(colnames(dta))
  colnames(dta) <- gsub("\\.\\.+", "\\.", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("\\.", "\\_", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("\\.$", "", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("^\\s", "\\1", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("\\s$", "\\1", colnames(dta), perl = TRUE)

  readr::write_rds(dta, "rds/dta_colnames_clean.rds")

  return(dta)
  # assign("dta", dta, envir = .GlobalEnv)

}

