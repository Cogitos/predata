% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/reportSE.R
\name{reportSE}
\alias{reportSE}
\title{Report descriptive statistics with standard errors}
\usage{
reportSE(data = NULL, dv, groupvars = NULL, conf.interval = 0.95)
}
\arguments{
\item{data}{A data frame in the long format}

\item{dv}{A string indicating the column name of the dependent variable (what is measured, recorded)}

\item{conf.interval}{The rate to use to compute the confident interval. Defaults to .95}

\item{wid}{A string indicating the column name of the subjects (or observations)}

\item{within}{A vector of string of column names of the within variables (repeated within
the same observation)}

\item{between}{A vector of string of column names of the between variables (across observations)}
}
\value{
Return a data frame of the number of observation (N), mean, standard deviation (sd),
corrected standard error (se), and confident interval (ci) by conditions.
}
\description{
A function cannibalized and slightly simplified from
\url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/}
}
\details{
The function compute simple descriptive statistics: number of observations (N), mean,
standard deviation (sd), confident intervals (ci) and standard error (se).
}
\examples{
reportWithin()
}
\author{
Guillaume T. Vallet \email{gtvallet@gmail.com}, University of Montreal (Canada)
}
\seealso{
\code{\link{reportWithin}}
}
\keyword{summary}

