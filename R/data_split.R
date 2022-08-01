#' Split into participant and QC data
#'
#' @param dta data from symbols remove function.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr all_of
#' @export
#'
data_split <- function(dta = dta) {

  dta_participants <-
    c("date", "instrument", "kit",
      "plate", "rerun", "well", "description", "analyte",
      "analyte_simple", "consensus", "analyte_in_ref",
      "fi_bkgd_num", "fi_bkgd_meta", "conc_obs_num",
      "conc_obs_meta")

  dta_qc <-
    c("type", "date", "instrument", "kit",
      "plate", "rerun", "well", "description", "analyte",
      "analyte_simple", "consensus", "analyte_in_ref",
      "fi_bkgd_num", "fi_bkgd_meta", "conc_obs_num",
      "conc_obs_meta")

  dta_participants <- dta %>%
    dplyr::filter(str_detect(type, "X")) %>%
    dplyr::select(all_of(dta_participants))

  dta_qc <- dta %>%
    dplyr::filter(!str_detect(type,"X")) %>%
    dplyr::select(all_of(dta_qc))

  tmp <- list(dta_participants = dta_participants, dta_qc = dta_qc)
  readr::write_rds(tmp, "rds/dta_list.rds")

  # assign("dta_participants", dta_participants, envir = .GlobalEnv)
  # assign("dta_qc", dta_qc, envir = .GlobalEnv)
}
