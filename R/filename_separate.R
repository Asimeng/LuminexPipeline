#' filename_separate1 function not exported.
#'
#' @param data dta file from data_import function. it is assumed that the input data is already loaded in the environment.
#' if input data is not already in the environment, ensure that it is loaded or read it into the environment.
#'
filename_separate1 <- function(data = d) {

# Jesse's input: check to ascertain input data has the correct column for further processing, else throw an error.

  name <- deparse(substitute(data))

  if("filename" %in% names(data) == FALSE) {
    stop("check input data: input may not have a 'filename' column")

  }

#check if input data is loaded in the environment.
  # else if (ls(pattern = name) < 1){
  #   stop("cannot find input data: make sure input data is read into the environment")
  # }


  rr <- tibble::tibble(
    filename = unique(data$filename),
    filename_new = tolower(filename),
    kit = NA,
    instrument = NA,
    date = NA,
    plate1 = NA,
    plate = NA,
    rerun = NA
  )

  rr$filename_new <- gsub("\\.|\\/|\\-", "_", rr$filename_new)
  rr$filename_new <- gsub("\\_+", "_", rr$filename_new)
  rr$filename_new <- gsub("\\_$|^\\_", "", rr$filename_new)

  # Kit
  rr$kit <- gsub("([a-z0-9&]+)_([a-z]+)_([0-9]+)_(.*)",
                 "\\1",
                 rr$filename_new)

  # Instrument
  rr$instrument <- gsub("([a-z0-9&]+)_([a-z]+)_([0-9]+)_(.*)",
                        "\\2",
                        rr$filename_new)

  # Date
  rr$date <- gsub("(.*)_([[:alnum:]]*20[0-9][0-9][[:alnum:]]*)_(.*)",
                  "\\2",
                  rr$filename_new)

  # Plate nr
  rr$plate1 <- gsub("([a-z0-9&]+)_([a-z]+)_([0-9]+)_(.*)",
                    "\\4",
                    rr$filename_new)

  ######

  r <- rr$plate1

  r <- gsub("([a-z]+)", "_", r)

  r <- gsub("(^[[:punct:]]*)", "", r)

  r <- gsub("[_]+[0-9]+|[_]*", "", r)

  r <- ifelse(grepl("^[a-z]*$",
                    rr$plate1, perl = TRUE) == TRUE,
              NA, r)

  r <- gsub("(*)\\s+([0-9]+)", "\\1", r)

  r <- gsub("\\s+", "", r)

  rr$plate <- ifelse(is.na(r),
                     1, r)

  # rerun
  r <- rr$plate1

  r <- gsub("^[0-9]+$", NA, r)

  r <- gsub("^[0-9]+", "", r)

  r <- gsub('rerun|r', "", r)

  r <- gsubfn::gsubfn("\\w+",
                      stats::setNames(as.list(1:100),
                               tolower(english::as.english(1:100))), r)
  r[r %in% c("NA", "na")] <- NA

  r <- gsub("[0-9]*[_]", "", r)

  r <- gsub("\\s+", "", r)

  rr$rerun <- r

  rr$rerun <- ifelse(rr$rerun == "", 1, rr$rerun)

  rr$rerun <- ifelse(is.na(rr$rerun) == TRUE, 0, rr$rerun)

  ######

  dta <-  rr %>%
    dplyr::select(filename, date, kit, instrument, plate, rerun) %>%
    dplyr::inner_join(data)  %>%
    dplyr::distinct()
}


instrument_name_check <- function(d, instrument_names) {
  #' Check whether instrument_name is same as input instrument_name.
  #' @param d dataframe created by preceding function
  #' @param instrument_names character vector from essential metadata

  a <- which(!d$instrument %in% instrument_names)

  if (length(a) != 0 & length(a) != nrow(d)) {
    tmp <- tibble::tibble(instrument_names_unexpected = d$instrument[a],
                          in_filename = d$filename[a])
    readr::write_rds(tmp, "rds/rep_instr_unexpected.rds")
  }

  if (length(a) == nrow(d)) {
    tmp <- tibble::tibble(unexpected_instrument_names = unique(d$instrument))
    readr::write_rds(tmp, "rds/rep_instr_unexpected.rds")
  }

  if (sum(!d$instrument %in% instrument_names) == 0) {
    tmp <- tibble::tibble(instrument_names_unexpected = NA)
    readr::write_rds(tmp, "rds/rep_instr_unexpected.rds")
  }
}


dates_filename <- function(d) {
  #' Check date format in FileName
  #' @param d Dataset created by preceding function

  dd <- lubridate::parse_date_time(d$date, orders = c("ymd", "dmy", "mdy"), train = TRUE)

  # If output created, check for NAs
  # If no NAs, print unique dates
  # If NAs, print the ones that produced NAs
  # If no output created or error - message

  if(exists("dd") == FALSE){
    dates_info <- "No dates could be parsed"
  } else if(sum(is.na(dd)) == length(d$date) | is.null(dd) == TRUE){
    dates_info <- "No dates could be parsed"
  } else if(sum(is.na(dd)) != 0 & sum(is.na(dd) != length(d$date))){
    dates_info <- list(unique(dd), d$date[is.na(dd)])
    names(dates_info) <- c("Correctly_parsed_unique_ dates", "Incorrectly_parsed_dates")
  } else if(sum(is.na(dd)) == 0){
    dates_info <- list(Correctly_parsed_unique_dates = unique(dd))
    d$date <- dd
    readr::write_rds(dates_info, "rds/rep_dates_info.rds")
    assign("d", d, envir = .GlobalEnv)
  }
  return(d)
}



#' Extract relevant metadata from filenames of valid imported Luminex files
#'
#' @description
#' Extract useful metadata e.g., (kit name, instrument name, date) from
#' the \code{filenames} column created by the \code{data_import()} function
#' and assign each metadata type a column in the output tibble.
#'
#' @param data Output of the \code{data_import()} function.
#' A valid Luminex tibble or dataframe.
#'
#'
#' @param instrument_names Character vector of expected instrument names.
#' This is sourced from the accompanying metadata from the experiment
#'
#' @details
#' As part of the standard operating procedures (SOP), The raw Luminex
#' \code{.txt} files are named with metadata with a specified guideline. This
#' function extracts these metadata (from the \code{filenames} column created
#' by the \code{data_import()} function) and adds new columns to
#' the output tibble for each type of metadata e.g., "date", "instrument_name" etc.
#' i.e, values of these new columns are extracted from \code{filename} column of the
#' input data.
#'
#' The new columns are parsed with its correct data types. e.g., dates are parsed
#' with date-time format.
#'
#' The \code{filename_separate()} function works with the assumption that its
#' input data is available in the R environment. If input data is not already
#' available in the environment, ensure that it is loaded in the environment
#'
#' @return A tibble of the input data with added columns for extracted metadata
#' e.g., date, kit, instrument, plate and rerun.
#'
#' @examples
#' #assign instrument names to a variable called ins_names
#' ins_names <- c("bp", "rd")
#'
#' #read input data to the environment
#' my_data <- readRDS("tests/testthat/rds/dta_import.rds")
#'
#' filename_separate(data = my_data, instrument_names = ins_names)
#'
#' @export
#'

filename_separate <- function(data = dta,
                              instrument_names = instrument_names){

  d <- data
  # 1. separate filename

  d <- filename_separate1(data = d)


  # 2. check instrument names

  instrument_name_check(d, instrument_names)

  # 2.  check date format

  d <- dates_filename(d)

  readr::write_rds(d, "rds/dta_separate.rds")

  return(d)


}

