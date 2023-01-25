#' Summary table
#'
#' @param dta processed dataframe
#' @param conc_col column name for observed concentrations in "quotes"
#' @param analyte_col column name for analyte in "quotes"
#' @return a table of summaries
#' @export
#'
#'
summ_table <- function(dta, analyte_col = "analyte", conc_col = "obs_conc") {

  options(scipen = 999, digits = 1)

  summary_table <- dta %>%

    group_by(.data[[analyte_col]]) %>%

    dplyr::summarise(
      minimum = min(.data[[conc_col]], na.rm = TRUE),
      maximum = max(.data[[conc_col]], na.rm = TRUE),
      mean = mean(.data[[conc_col]], na.rm = TRUE),
      first_quart = quantile(.data[[conc_col]],  probs = 0.25, na.rm = TRUE),
      median = median(.data[[conc_col]], na.rm = TRUE),
      third_quart = quantile(.data[[conc_col]], probs = 0.75, na.rm = TRUE),
      NAs = sum(is.na(.data[[conc_col]])),
      available_obs = sum(!is.na(.data[[conc_col]])),
      total_obs = n()) %>%

    ungroup()

  #readr::write_rds(summary_table, "rds/summary_table.rds")

  return(summary_table)

}



#' Histogram plot
#'
#' @param dta_prtc processed dataframe
#' @param analyte_col column name for analyte in "quotes"
#' @param conc_col column name for observed concentration in "quotes"
#' @param facet_rows numeric value indicating number of rows to facet.
#'
#' @return a plot of histogram
#' @export
#'
#'
#'
histogram_plot <- function(dta_prtc, analyte_col = "analyte", conc_col = "obs_conc",  facet_rows = 5) {

  dta_prtc %>%
    ggplot2::ggplot(aes(x = .data[[conc_col]])) +
    geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "grey", na.rm = TRUE) +
    geom_density(colour = "blue") +
    facet_wrap(~ .data[[analyte_col]], nrow = facet_rows,  scales ="free") +
    ggtitle("Plot with untransformed data")
}



#' Test for normality
#'
#' @param dat data to generate test of normality table from
#' @param analyte_col column name for analyte in "quotes"
#' @param conc_col column name for observed concentration in "quotes"
#' @return a table of normality test
#' @export
#'
#'
norm_tab <- function(dat, analyte_col = "analyte", conc_col = "obs_conc"){

  options(scipen = F, digits = 1)

  tab <-  dat %>%

    group_by(.data[[analyte_col]]) %>%

    dplyr::summarise(kurtosis = DescTools::Kurt(.data[[conc_col]], na.rm = T),

              skewness = DescTools::Skew(.data[[conc_col]], na.rm = T),

              Shapiro.Francia = if(sum(!is.na(.data[[conc_col]])) >= 5) {

                DescTools::ShapiroFranciaTest(.data[[conc_col]])[["statistic"]]

              } else {NA
              },

              p_val = if(sum(!is.na(.data[[conc_col]])) >= 5) {

                DescTools::ShapiroFranciaTest(.data[[conc_col]])[["p.value"]]

              } else{NA
              }
    )

  return(tab)
}



#' Generate quantile-quantile plots to assess normality
#'
#' @param dta data to generate q-q plot from from
#' @param analyte_col column name for analyte in "quotes"
#' @param conc_col column name for observed concentration in "quotes"
#' @param facet_row numeric value indicating number of rows to facet
#'
#' @return A quantile-quantile graph
#' @export
#'
#'
qq_plot <- function(dta, analyte_col = "analyte", conc_col = "obs_conc", facet_row = 5){

  dta %>%
    ggplot2::ggplot(aes(sample = .data[[conc_col]])) +
    stat_qq() +
    stat_qq_line() +
    ggplot2::facet_wrap(~ .data[[analyte_col]], nrow = facet_row, scales = "free")
}



#' Comparison between groups being analysed
#'
#' @param dta_prtc participant data
#' @param dta_clin clinical data
#'
#' @return A dataframe of comparison between groups summary
#' @export
#'
#'
grp_test <- function(dta_prtc, dta_clin){

  grp_dat <- inner_join(dta_prtc, dta_clin, by = "description") %>%
    select(analyte, conc_obs_num, age, group) %>%
    mutate(group = as.factor(group)
    ) %>%

# Analytes with a single observation eg (IL-6 of test dataset) returns an error. work around to filter such data

    group_by(analyte) %>%

    dplyr::summarise(p.value = wilcox.test(conc_obs_num ~ group )[["p.value"]],

              w.statistic = wilcox.test(conc_obs_num ~ group)[["statistic"]])

  return(grp_dat)
}



#' Correlation tests
#'
#' @param dta dataframe from the pipeline
#' @param analyte_col column name for analyte in "quotes"
#' @param conc_col column name for observed concentration in "quotes"
#' @param cor_type type of correlation in string format. eg "pearson"
#'
#' @importFrom Hmisc rcorr
#'
#' @return a flatten correlation matrix (table)
#' @export
#'
#'
correlation <- function(dta, analyte_col = "analyte", conc_col = "obs_conc", cor_type = "spearman"){

  piv_dat <- dta %>%

    select(.data[[analyte_col]], .data[[conc_col]]) %>%

    group_by(.data[[analyte_col]]) %>%

    mutate(row = row_number()) %>%

    tidyr::pivot_wider(
      names_from = .data[[analyte_col]],
      values_from = .data[[conc_col]]) %>%

    dplyr::select(-row)

  cor <- Hmisc::rcorr(as.matrix(piv_dat), type = cor_type)

  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      analyte_1 = rownames(cormat)[row(cormat)[ut]],
      analyte_2 = rownames(cormat)[col(cormat)[ut]],
      co.eff  = (cormat)[ut],
      p.value = pmat[ut]
    )
  }

  cor <- flattenCorrMatrix(cor$r, cor$P)

  arr_cor <- arrange(cor, desc(abs(co.eff))) %>%
    filter(!is.na(co.eff))

  return(arr_cor)
}



#' Visualise correlogram - analytes
#'
#' @param dta dataframe from pipeline
#' @param analyte_col column name for analyte in "quotes"
#' @param conc_col column name for observed concentration in "quotes"
#' @param cor_type character, type of correlation to compute
#'
#' @importFrom Hmisc rcorr
#'
#' @return A graph (correlogram) of correlations
#' @export
#'
#'
cor_plot <- function(dta, analyte_col = "analyte", conc_col = "obs_conc", cor_type = "spearman"){

  piv_dat <- dta %>%

    select(.data[[analyte_col]], .data[[conc_col]]) %>%

    group_by(.data[[analyte_col]]) %>%

    mutate(row = row_number()) %>%

    tidyr::pivot_wider(
      names_from = .data[[analyte_col]],
      values_from = .data[[conc_col]]) %>%

    dplyr::select(-row)


  #res <- cor(piv_dat)

  cor <- Hmisc::rcorr(as.matrix(piv_dat), type = cor_type)

  #round(res, 2)
  #corrplot(res, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)

  corrplot::corrplot(cor[["r"]], type="upper", order="alphabet",
                     tl.cex = 0.6,tl.col="black", tl.srt=45)
}


#Render statistical summary report

# rmarkdown::render(
#   input = paste0(system.file(package = "LuminexPipeline"), "/extdata/rmd/reports.Rmd"),
#   output_file = "statistical_report.html",
#   params = list(cor_type = "pearson", facet = 9, data = `6_dta_symbol_remove`),
#   encoding     = 'UTF-8'
# )
