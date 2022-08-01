
#' Fix analyte names
#'
#' @param analyte_reference From external table or database
#' @param dta returned data from clean colnames function
#'
#' @import dplyr stringr
#' @export
#'
analyte_names_fix <- function(analyte_reference = analyte_reference,
                              dta = dta) {
  #Split analyte and bead colour
  tmp <- stringr::str_split_fixed(dta$analyte, "\\(", 2)
  colnames(tmp) <- c("analyte", "bead_colour")
  tmp <- as.data.frame(tmp)

  dta <- dta %>%
    select(-analyte) %>%
    bind_cols(tmp)

  # join to analyte_ref
  dta <- analyte_reference %>%
    mutate(tmp = analyte %in% unique(dta$analyte)) %>%
    filter(tmp == TRUE) %>%
    select(-tmp) %>%
    full_join(dta) %>%
    distinct() %>%
    rename(analyte_orig = analyte,
           analyte = analyte_new)

  readr::write_rds(dta, "rds/dta.rds")
  # assign("dta", dta, envir = .GlobalEnv)

}

# not exported
split_beadnr <- function(dta = dta){

  tmp <- stringr::str_split_fixed(dta$analyte, "\\(", 2)
  colnames(tmp) <- c("analyte", "bead_colour")
  tmp <- as.data.frame(tmp)

  dta <- dta %>%
    select(-analyte) %>%
    bind_cols(tmp)

}


# not exported

# symbol_codes

clean_Analyte4 <- function(v = NULL) {

  v <- gsub("(.*)(/)(.*)", "\\1", v)
  v <- gsub("(.*)(/)(.*)", "\\1", v)
  v <- gsub("(.*)(/)(.*)", "\\1", v)
  v <- gsub("(.*)(/)(.*)", "\\1", v)

  v <- gsub("'", "", v)
  v <- gsub("’", "", v)
  v <- gsub("_", "", v)

  v <- gsub("\\", "", v, fixed = TRUE)
  v <- gsub("\\([^\\)]+\\)", "", v, perl = TRUE)
  v <-
    gsub("[\\/]", "", v, perl = TRUE)
  v <- gsub("([-])([0-9]+)", "\\2", v, perl = TRUE)
  v <- gsub("-", "", v, perl = TRUE)
  v <- gsub("  *$", "", v, perl = TRUE)
  v <- gsub("\\s+$", "", v, perl = TRUE)
  v <- gsub("\\s+", "", v, perl = TRUE)
  v <- gsub("^_", "", v, perl = TRUE)
  v <- gsub("_$", "", v, perl = TRUE)
  v <- gsub("'", "", v, perl = TRUE)
  v <- gsub(",", "", v, perl = TRUE)

  v <- tolower(v)

  return(v)

}


#' Fix analyte names
#'
#' @param analyte_reference_path reference path
#' @param dta dta path from colname_clean function
#'
#' @import dplyr tidyr
#' @export
#'
analyte_names_fix2 <- function(analyte_reference_path = analyte_reference_path,
                               dta = dta) {

  d <- split_beadnr(dta)
  x <- clean_Analyte4(d$analyte)
  sort(unique(x))

  d$analyte_simple <- x

  # anref <- readr::read_csv(analyte_reference_path)

  x <- unique(d$analyte_simple)
  xx <- which(!x %in% lum_analyte_ref$analyte_simple) # lum_analyte_ref loaded when package loads

  d <- d %>%
    mutate(analyte_in_ref = ifelse(!d$analyte_simple %in% x[xx],
                                   TRUE, FALSE))

  d <- d %>%
    left_join(lum_analyte_ref %>%
                select(analyte_simple, consensus)) %>%
    distinct() %>%
    mutate(across(consensus,
                  ~case_when(is.na(consensus) ~ analyte_simple,
                             TRUE ~ consensus)))

  readr::write_rds(d, "rds/dta_analyte_ref.rds")

}
