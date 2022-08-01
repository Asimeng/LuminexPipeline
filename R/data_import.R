#' data_import function Reads in valid files, one line per well, keeping standards,
#' controls and unknowns in one data frame called "dta"
#'
#' @param dir_data directory of datasets
#' @importFrom magrittr %>%
#' @importFrom utils read.delim
#'
#' @return None
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
