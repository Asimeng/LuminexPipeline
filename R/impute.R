
# impute OOR
# max plus 0.05 of max
# min - 0.05 of min
# can add other methods later

#' Impute out-of-range concentration values
#'
#' @param dta_list dta_list file from data_split function
#' @import dplyr
#' @return NA
#' @export
#'
conc_oor_impute <- function(dta_list = dta_list) {

  dta <- dta_list$dta_participants

  tmp <- dta %>%
    group_by(instrument, consensus) %>%
    summarise(
      min_x = min(conc_obs_num, na.rm = TRUE),
      max_x = max(conc_obs_num, na.rm = TRUE),
      min_xa = min_x - (min_x * 0.05),
      max_xa = max_x + (max_x * 0.05)
    ) %>% # add different methods here
    ungroup()

  # add if statement for config specification of imputation type here, later

  dta_list$dta_participants <- dta %>%
    inner_join(tmp) %>%
    mutate(
      conc_obs_imp = case_when(
        conc_obs_meta == "OOR <" ~ min_xa,
        conc_obs_meta == "OOR >" ~ max_xa,
        TRUE ~ conc_obs_num
      )
    ) %>%
    select(-contains("min_x"),-contains("max_x"))

  readr::write_rds(dta_list, "rds/dta_list_impute.rds")

}
