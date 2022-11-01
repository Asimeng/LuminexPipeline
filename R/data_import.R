#' Reads and import raw Luminex .txt files into R
#'
#' @description
#' Recursively searches input directory for valid Luminex .txt files and converts
#' them into an rds format. The rds file is then written to the \code{rds/}
#' directory configured with \code{pipeline_config()} function.
#'
#' @param dir_data Path or directory to input datasets (\code{.txt} files)
#'
#' @importFrom magrittr %>%
#' @importFrom utils read.delim
#'
#' @details
#' The input directory may contain files other than .txt file formats. In such
#' case, this function will process these non-compliant files types and save
#' them in an \code{.rds} file format.
#'
#' The function checks for expected column names of standard Luminex files e.g.,
#' \code{("Analyte","Type", "Well","Outlier","Description","FI")}
#'  in the imported \code{.txt} files and classifies them as valid or invalid
#'   Luminex files.
#'
#' All valid Luminex files are merged into a single \code{.rds} file and written
#'  to a file named: \code{rds/dta_import.rds/}
#'
#' All invalid Luminex files are stored in a single \code{.rds} file and written
#'  to a file named: \code{rds/rep_files_invalid.rds/}
#'
#' The \code{rds/} directory can be manually created in the working directory as
#' an alternative to using the \code{pipeline_config()} function to set it up.
#'
#' The function creates a \code{filename} column with extracted data from the raw
#' Luminex \code{.txt} filenames.
#'
#' @return An unprocessed (raw) tibble of valid Luminex files that are binded
#' by rows.
#'
#' @examples
#' dir <- "inst/extdata/"
#'
#' data_import(dir_data = dir)
#'
#' @export
#'

data_import <- function(dir_data) {

  files <- list.files(dir_data, recursive = TRUE,
                      full.names = TRUE)

#Jesse's input: stop if input directory is empty

  if (length(files) < 1) {
    stop("check input directory: misspelt or empty directory")
  }

  luminex_list <- list()

  files_invalid <- tibble::tibble(
    files = files,
    txt = NA,
    line_first = NA,
    header_repeat = NA
  )

  names_check <-
    c("Analyte",
      "Type",
      "Well",
      "Outlier",
      "Description",
      "FI",
      "FI...Bkgd")

  for (i in 1:length(files)) {

    if(grepl(".txt", files[i]) == FALSE){

      files_invalid$line_first[i] <- 0
      files_invalid$header_repeat[i] <- 0
      files_invalid$txt[i] <- 0

#Jesse's input: warning on incorrect file extensions (other than .txt)

      warning("some files may have incorrect format")

    } else if(grepl(".txt", files[i]) == TRUE){ #opens1

      plate <-
        read.delim(files[i],
                   stringsAsFactors = FALSE,
                   strip.white = TRUE)

      if(ncol(plate) > 2){ #opens2

        if(!is.null(names(plate))){
          line_first <- unlist(names(plate))
        }

        files_invalid$line_first[i] <-
          sum(names_check %in% line_first) == length(names_check)


        x <- grep("Analyte", plate[, 1])
        x1 <- grep("Well", plate[,3])
        if(length(x) != 0 & length(x1) != 0){
          files_invalid$header_repeat[i] <- x == x1
        }
      } else{
        files_invalid$line_first[i] <- 0
        files_invalid$header_repeat[i] <- 0
        files_invalid$txt[i] <- 1
      }
      # closes2

      files_invalid$txt[i] <- grepl(".txt", files[i])


      x2 <- sum(files_invalid$line_first[i],
                files_invalid$header_repeat[i],
                files_invalid$txt[i])

      if(is.null(x2) | is.na(x2)){
        luminex_list[[i]] <- "invalid"

      } else if (x2 == 3) {
        plate <- plate[(x + 1):nrow(plate), ]

        plate$filename <-
          sub("\\..*$", "", sub("^.*[/\\]", "", files[i]))
        # remove path and extension from filename

        luminex_list[[i]] <- plate
      } else{
        luminex_list[[i]] <- "invalid"
      }
    } #closes1
  }

  files <- dplyr::filter(files_invalid,
                         txt == TRUE &
                           line_first == TRUE &
                           header_repeat == TRUE) %>%
    dplyr::pull(files)
  readr::write_rds(files, "rds/rep_files.rds")
  files_invalid <- dplyr::filter(files_invalid,
                                 txt == FALSE |
                                   line_first == FALSE |
                                   header_repeat == FALSE)
  readr::write_rds(files_invalid, "rds/rep_files_invalid.rds")

  luminex_list <- luminex_list[luminex_list != "invalid"]
  dta <- dplyr::bind_rows(luminex_list)
  readr::write_rds(dta, "rds/dta_import.rds")
  return(dta)
}
