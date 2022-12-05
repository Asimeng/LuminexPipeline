#' Summary table
#'
#' @param dta processed dataframe
#'
#' @return a table of summaries
#' @export
#'
#'
summ_table <- function(dta) {

  options(scipen = 999, digits = 1)

  summary_table <- dta %>%

    group_by(analyte) %>%

    dplyr::summarise(
      minimum = min(conc_obs_num, na.rm = TRUE),
      maximum = max(conc_obs_num, na.rm = TRUE),
      mean = mean(conc_obs_num, na.rm = TRUE),
      first_quart = quantile(conc_obs_num,  probs = 0.25, na.rm = TRUE),
      median = median(conc_obs_num, na.rm = TRUE),
      third_quart = quantile(conc_obs_num, probs = 0.75, na.rm = TRUE),
      NAs = sum(is.na(conc_obs_num)),
      available_obs = sum(!is.na(conc_obs_num)),
      total_obs = n()) %>%

    ungroup()

  #readr::write_rds(summary_table, "rds/summary_table.rds")

  return(summary_table)

}



#' Histogram plot
#'
#' @param dta_prtc processed dataframe
#' @param facet_rows numeric value indicating number of rows to facet.
#'
#' @return a plot of histogram
#' @export
#'
#'
#'
histogram_plot <- function(dta_prtc, facet_rows = 5) {

  dta_prtc %>%
    ggplot2::ggplot(aes(x = conc_obs_num)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "grey", na.rm = TRUE) +
    geom_density(colour = "blue") +
    facet_wrap(~ analyte, nrow = facet_rows,  scales ="free") +
    ggtitle("Plot with untransformed data")
}



#' Test for normality
#'
#' @param dat data to generate test of normality table from
#'
#' @return a table of normality test
#' @export
#'
#'
norm_tab <- function(dat){

  options(scipen = F, digits = 1)

  tab <-  dat %>%

    group_by(analyte) %>%

    dplyr::summarise(kurtosis = DescTools::Kurt(conc_obs_num, na.rm = T),

              skewness = DescTools::Skew(conc_obs_num, na.rm = T),

              Shapiro.Francia = if(sum(!is.na(conc_obs_num)) >= 5) {

                DescTools::ShapiroFranciaTest(conc_obs_num)[["statistic"]]

              } else {NA
              },

              p_val = if(sum(!is.na(conc_obs_num)) >= 5) {

                DescTools::ShapiroFranciaTest(conc_obs_num)[["p.value"]]

              } else{NA
              }
    )

  return(tab)
}



#' Generate quantile-quantile plots to assess normality
#'
#' @param dta dataframe from pipeline
#' @param facet_row numeric value indicating number of rows to facet
#'
#' @return A quantile-quantil graph
#' @export
#'
#'
qq_plot <- function(dta, facet_row = 5){

  dta %>%
    ggplot2::ggplot(aes(sample = conc_obs_num)) +
    stat_qq() +
    stat_qq_line() +
    ggplot2::facet_wrap(~ analyte, nrow = facet_row, scales = "free")
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
#' @param cor_type type of correlation in string format. eg "pearson"
#'
#' @importFrom Hmisc rcorr
#'
#' @return a flatten correlation matrix (table)
#' @export
#'
#'
correlation <- function(dta, cor_type = "spearman"){

  piv_dat <- dta %>%

    select(analyte, conc_obs_num) %>%

    group_by(analyte) %>%

    mutate(row = row_number()) %>%

    tidyr::pivot_wider(
      names_from = analyte,
      values_from = conc_obs_num) %>%

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
#' @param cor_type character, type of correlation to compute
#'
#' @importFrom Hmisc rcorr
#'
#' @return A graph (correlogram) of correlations
#' @export
#'
#'
cor_plot <- function(dta, cor_type = "spearman"){

  piv_dat <- dta %>%

    select(analyte, conc_obs_num) %>%

    group_by(analyte) %>%

    mutate(row = row_number()) %>%

    tidyr::pivot_wider(
      names_from = analyte,
      values_from = conc_obs_num) %>%

    dplyr::select(-row)


  #res <- cor(piv_dat)

  cor <- Hmisc::rcorr(as.matrix(piv_dat), type = cor_type)

  #round(res, 2)
  #corrplot(res, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)

  corrplot::corrplot(cor[["r"]], type="upper", order="alphabet",
                     tl.cex = 0.6,tl.col="black", tl.srt=45)
}

#' render statistical summary report
#'
#' @param dta rds file with its extension
#'
#' @return an html report
#' @export
#'
#'

render_report <- function(dta) {

  rmarkdown::render(
    input = "vignettes/reports.Rmd",
    output_dir = "rds/statistical_summary_report.html",
    params = list(
      directory = "rds",
      file = dta
    )
  )

}
