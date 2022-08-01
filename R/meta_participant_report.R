# nr of participants
# check list against metadata
# exposure / intervention groups
# check plate layouts vs exposure/intervention groups

# nr plus list of analytes, kits
# analytes not in reference

# run dates

# kits, plates, instruments

# reruns

# nrs of observations per analyte per participant




#' Create metadata report and compare to ISA data
#'
#' @param dta_list data list from data_split function
#' @param isa_path path to ISA files
#' @param config_isa path to config ISA
#' @import purrr
#' @return isa_report_list.rds
#' @export
#'
report_isa <- function(dta_list = dta_list,
                       isa_path = isa_path,
                       config_isa = config_isa){

  dta <- dta_list$dta_participants

  if(config_isa == TRUE){ # open 1

    isa <- readr::read_rds(isa_path)

    report_list <- list(participant_discrepancies = NA,
                        exposure_intervention_groups = NA,
                        kit_plate_analyte = NA,
                        analyte_in_ref = NA,
                        in_isa_kit_analyte = NA,
                        in_isa_date = NA)

    report_list$participant_discrepancies <-
      tibble(id = sort(unique(dta$description)),
             source_dta = 1) %>%
      full_join(tibble(id = sort(unique(isa$participant_list)),
                       source_isa = 1)) %>%
      mutate(source_sum = source_dta + source_isa) %>%
      filter(source_sum != 2 | is.na(source_sum))

    grps <- isa$participant_groups_nr

    d <- dta %>%
      select(description) %>%
      distinct() %>%
      filter(description %in% isa$participant_list)

    report_list$exposure_intervention_groups <-
      purrr::map(1:grps,
                 ~d %>%
                   mutate(participant_group =
                            case_when(description %in% isa$participant_groups_list[[.x]] ~
                                        names(isa$participant_groups_list)[.x])) %>%
                   filter(!is.na(participant_group))) %>%
      bind_rows()


    report_list$kit_plate_analyte <- dta %>%
      select(kit, plate, consensus) %>%
      distinct() %>%
      arrange(kit, plate, consensus) %>%
      mutate(n = 1) %>%
      tidyr::pivot_wider(names_from = plate, values_from = n) %>%
      print(n = Inf)

    report_list$in_isa_kit_analyte <-
      report_list$kit_plate_analyte %>%
      select(kit, consensus) %>%
      mutate(in_isa_kit = case_when(
        kit %in% isa$kit_list ~ TRUE,
        TRUE ~ FALSE),
        in_isa_analyte = case_when(
          consensus %in% isa$analyte_list ~
            TRUE,
          TRUE ~ FALSE))

    report_list$analyte_in_ref <- dta %>%
      select(analyte, consensus, analyte_in_ref) %>%
      distinct() %>%
      filter(analyte_in_ref == FALSE)

    report_list$in_isa_date <- dta %>%
      distinct(date) %>%
      mutate(in_isa_date = case_when(
        dates %in% isa$run_dates ~ TRUE,
        TRUE ~ FALSE
      )) %>%
      filter(in_isa_date == FALSE)


    report_list$instrument_kit_plate <-
      dta %>%
      distinct(kit, plate, instrument) %>%
      mutate(n = 1) %>%
      arrange(plate, instrument, kit) %>%
      tidyr::pivot_wider(names_from = plate, values_from = n) %>%
      print(n = Inf)

    report_list$in_isa_instrument <-
      dta %>%
      distinct(instrument) %>%
      mutate(in_isa_instrument = case_when(
        instrument %in% isa$instrument ~ TRUE,
        TRUE ~ FALSE
      )) %>%
      filter(in_isa_instrument == FALSE)


    report_list$reruns <-
      dta %>%
      select(description, instrument, kit,
             plate, well, consensus, rerun) %>%
      filter(rerun != 0)

    # not sure what form isa reruns will take?



    # nrs of observations per analyte per participant

    report_list$obs_per_analyte_per_participant <-
      dta %>%
      group_by(description, consensus) %>%
      count() %>%
      arrange(description, consensus) %>%
      tidyr::pivot_wider(names_from = consensus,
                         values_from = n)

  } else{# close 1
    report_list <- "ISA comparison not specified in config file"
  }

  readr::write_rds(report_list, "rds/isa_report_list.rds")
}
