# some concentrations will have one star and a value
# some will have three stars, no value
# some will have only a value

# dta$obs_conc[grepl("\\*", dta$obs_conc)]
# dta$obs_conc[grepl("\\*\\*", dta$obs_conc)]
# dta$fi_bkgd[grepl("\\*", dta$fi_bkgd)]
# dta$obs_conc[grepl("\\*\\*", dta$fi_bkgd)]


#' Remove symbols from \code{concentration} and \code{FI} columns
#'
#' @description
#' Cleans the machine annotated "*" symbols (out-of-range values) in the
#' \code{concentration} and \code{FI} columns and then parses the columns
#' with numeric data types. Also annotates the out-of-range metadata in a new
#' \code{fi_bkgd_meta} column.
#'
#' @param dta Output tibble or dataframe of the preceding
#' \code{analyte_names_fix()} function.
#'
#' @import dplyr readr
#'
#' @details
#' The \code{symbols_remove()} function assumes its input data is available
#' in the R environment. if input data is not already available in the environment,
#' ensure that it is loaded in the environment.
#'
#' @export
#'
symbols_remove <- function(dta = dta) {
  dta <- dta %>%
    dplyr::mutate(tmp = gsub("\\*", "", obs_conc),
                  conc_obs_num = as.numeric(tmp),
                  conc_obs_meta = dplyr::case_when(
                    grepl("\\*+", obs_conc) == TRUE ~
                      gsub("[0-9, \\.]", "", obs_conc),
                    is.na(conc_obs_num) == FALSE ~ "in_range",
                    TRUE ~ obs_conc),
                  tmp1 = gsub("\\*", "", fi_bkgd),
                  fi_bkgd_num = as.numeric(tmp1),
                  fi_bkgd_meta = dplyr::case_when(grepl("\\*+", fi_bkgd) == TRUE ~
                                                    gsub("[0-9, \\.]", "", fi_bkgd),
                                                  is.na(fi_bkgd_num) == FALSE ~ "in_range",
                                                  TRUE ~ fi_bkgd)) %>%
    dplyr::select(-tmp, -tmp1)

  readr::write_rds(dta, "rds/6_dta_symbol_remove.rds")
}
