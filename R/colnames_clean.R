#' Clean column names
#'
#' @description
#' cleans column names of input data into recommended standard compliant form
#' e.g., transforming column names to lower case,
#' removal of special characters in column names, etc.
#'
#' @param dta output tibble or dataframe of the preceding
#'  \code{filename_separate()} function.This can as well be any other dataframe
#'
#' @details
#' The \code{colnames_clean()} function assumes its input data is available
#' in the R environment. if input data is not already available in the environment,
#' ensure that it is loaded in the environment.
#'
#' Output of this function is written to a file named:
#' \code{rds/dta_colnames_clean.rds}
#'
#' This function can be applied to other imported datasets.
#'
#' @return a tibble of the input data transformed with cleaned column names.
#'
#' @examples
#' #read data into the environment
#' data(dta_separate)
#' dir.create("rds")
#' colnames_clean(dta = dta_separate)
#'
#' #delete the rds directory created
#' unlink("rds")
#'
#' @export
#'

colnames_clean <- function(dta){

  colnames(dta) <- tolower(colnames(dta))
  colnames(dta) <- gsub("\\.\\.+", "\\.", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("\\.", "\\_", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("\\.$", "", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("^\\s", "\\1", colnames(dta), perl = TRUE)
  colnames(dta) <- gsub("\\s$", "\\1", colnames(dta), perl = TRUE)

  readr::write_rds(dta, "rds/3_dta_colnames_clean.rds")

  return(dta)
  # assign("dta", dta, envir = .GlobalEnv)

}

