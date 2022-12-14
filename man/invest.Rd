% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drL_invest.R
\name{invest}
\alias{invest}
\title{Estimate the concentration given a response value}
\usage{
invest(x, analyte=NULL, yvalue, ci.method = c("delta", "bootstrap"),
        level = 0.95, seed.boot = 123, nboot = 100)
}
\arguments{
\item{x}{a \code{scluminex} object.}

\item{analyte}{the specific analyte to estimate the invert values.
Default \code{NULL} (all analytes).}

\item{yvalue}{value of the response model to estimate the inverse in
log10 scale.}

\item{ci.method}{character defining the method to be applied for estimating
standard error ('delta' or 'bootstrap'). Default 'delta'.}

\item{level}{confidence level. Default 0.95.}

\item{seed.boot}{numeric for the seed of the bootstrap method. Only applies
for bootstrap method. Default 123.}

\item{nboot}{number of bootstrap replicates. Only applies for
bootstrap method. Default 100.}
}
\value{
A \code{data.frame} with the following components:
\itemize{
\item{MFI variable}{, the \code{yvalue} response vector}
\item{Fit of the concentration}{, concentration estimation of
the \code{yvalue} vector}
\item{Fit of the concentration.lci}{, lower confidence bounds
for the concentration estimation}
\item{Fit of the concentration.uci}{, upper confidence bounds
for the concentration estimation}
\item{Fit of the concentration.se}{, estimation of the Standard
Error of the concentration. If \code{ci.method} 'bootstrap' is \code{NA}}
}
}
\description{
Estimates the inverse of the funtion. Given a response value,
estimates the corresponding concentration value and the standard error.
NB: This is a borrowed function from the drLumi R package which
is no longer available on the CRAN for automatic installation.
}
\details{
Delta method function used is \code{\link[msm]{deltamethod}}
from the \code{msm} package.
Bootstrap method generates \code{nboot} response vectors
(assuming normality) and fit the same model with
original concentration data. The confidence interval is calculated
by the percentile method specified in the \code{level} argument
(\code{1-level}/2, 1-\code{(1-level)}/2).
}
\examples{
# Load data
data(ecdata)
data(mfidata)

dat <- mfidata[mfidata$plate=="plate_1" & mfidata$analyte=="FGF",]

# Estimate models
sdf <- data_selection(dat, ecdata)[[1]]
igmodels <- scluminex("plate_1",sdf$standard, sdf$background,
lfct="SSl4", bkg="ignore", fmfi="mfi", verbose=FALSE)

# Delta
invest(igmodels, "FGF", c(2, 2.5, 3),  "delta")

# Bootstrap
invest(igmodels, "FGF" ,c(2, 2.5, 3), "bootstrap", nboot=10)


}
