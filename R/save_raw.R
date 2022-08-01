#  Save the dataset in its current state,
# before removing columns, making numeric,
# removing symbols from obs_conc and imputing.


#' Save raw data file
#'
#' @param dta dataset from analyte_name_fix2 function
#' @import readr
#' @return a file named dta_raw.rds
#' @export
#'

save_raw <- function(dta = dta) {
  readr::write_rds(dta, "rds/dta_raw.rds")
}
