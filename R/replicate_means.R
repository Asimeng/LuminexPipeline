
# calculate means for replicates

replicate_means <- function(tech_reps = tech_reps,
                            dta_list = dta_list){

  x <- tech_reps # from config file

  # add nr of replicates as column
  # info for report ------------

  dta <- dta_list$dta_participants %>%
    distinct() %>%
    group_by(date, instrument,
             kit, plate, description, consensus, rerun) %>%
    summarise(nr_of_replicates = n()) %>%
    ungroup() %>%
    inner_join(dta_list$dta_participants)

  tmp <- dta %>%
    filter(nr_of_replicates != x)

  if(nrow(tmp) != 0){
    rep_tech_reps <- tmp
    readr::write_rds(rep_tech_reps, "rds/rep_tech_reps.rds")

  }

  # calculate means ----------------

  # can be
  # n > tech reps:  flag, don't calculate mean
  # n = 1 < tech reps:  use as is
  # n = tech reps:  calculate means

  dta_list$dta_participants <- dta %>%
    filter(nr_of_replicates == x) %>%
    group_by(date, instrument, kit, plate,
             rerun, description, analyte,
             analyte_simple, consensus,
             analyte_in_ref) %>%
    summarise(conc_obs_num_mean =
                mean(conc_obs_num, na.rm = TRUE)) %>%
    bind_rows(dta %>%
                filter(nr_of_replicates == 1)) %>%
    bind_rows(dta %>%
                filter(nr_of_replicates > x))

  readr::write_rds(dta_list, "rds/dta_list_replicate_means.rds")
}
