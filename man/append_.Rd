% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append_.R
\name{append_}
\alias{append_}
\title{Function to append new methods in the study}
\usage{
append_(object, Method, MethodName, ePara, ePara_name)
}
\arguments{
\item{object}{as output of 'prediction_errors()' function}

\item{Method}{as the list of locations of function for the proposed prediction method}

\item{MethodName}{as list of names for function for the proposed prediction method in order}

\item{ePara}{as type of error calculation (RMSE and MAE are default), add an error parameter of
your choice in the following manner: ePara = c("errorparametername")
where errorparametername is should be a source/function which returns desired error set}

\item{ePara_name}{as list of  names of error parameters passed in order}
}
\value{
Returns error comparison for additional forecasting methods
}
\description{
Function to append new methods in the study
}
\examples{
\dontrun{
library(forecast)
test3 <- function(data, nval){return(as.numeric(forecast(ets(data), h = nval)$mean))}
a <- prediction_errors(data = nottem)
b <- append_(object = a, Method = c("test3(data,nval)"), MethodName = c('ETS'))
choose_(object = a)
}
}
