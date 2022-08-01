#' pipeline_config
#'
#' @param wd work directory
#' @param dd data directory
#' @param md NA
#' @param aref analyte reference
#' @param isa_path path to isa
#' @param config_isa NA
#' @param tech_reps technical replicates
#'
#' @export
#'

pipeline_config <- function(wd, dd, md = NA,
                            aref, isa_path = NULL,
                            config_isa = FALSE,
                            tech_reps){

  assign("dir_main", wd, envir = .GlobalEnv)
  assign("dir_data", dd, envir = .GlobalEnv)
  assign("dir_md5sum", md, envir = .GlobalEnv)
  assign("analyte_reference_path", aref, envir = .GlobalEnv)
  assign("isa_path", isa_path, envir = .GlobalEnv)
  assign("config_isa", config_isa, envir = .GlobalEnv)
  assign("tech_reps", tech_reps, envir = .GlobalEnv)

  dir.create(file.path(dir_main, "rds"))
}
