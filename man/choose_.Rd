% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose_.R
\name{choose_}
\alias{choose_}
\title{Function to select the desired methods in the study}
\usage{
choose_(object)
}
\arguments{
\item{object}{as output of 'prediction_errors()' function}
}
\value{
Returns error comparison for selected forecasting methods
}
\description{
Function to select the desired methods in the study
}
\examples{
\dontrun{
a <- prediction_errors(data = nottem)
choose_(object = a)
}
}
