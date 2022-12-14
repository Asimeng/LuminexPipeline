% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symbol_remove.R
\name{symbols_remove}
\alias{symbols_remove}
\title{Remove symbols from \code{concentration} and \code{FI} columns}
\usage{
symbols_remove(dta = dta)
}
\arguments{
\item{dta}{Output tibble or dataframe of the preceding
\code{analyte_names_fix()} function.}
}
\description{
Cleans the machine annotated "*" symbols (out-of-range values) in the
\code{concentration} and \code{FI} columns and then parses the columns
with numeric data types. Also annotates the out-of-range metadata in a new
\code{fi_bkgd_meta} column.
}
\details{
The \code{symbols_remove()} function assumes its input data is available
in the R environment. if input data is not already available in the environment,
ensure that it is loaded in the environment.
}
