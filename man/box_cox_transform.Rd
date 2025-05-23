% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_cox_transform.R
\name{box_cox_transform}
\alias{box_cox_transform}
\title{Box-Cox Transformation for Numeric Data}
\usage{
box_cox_transform(data, type = c("optimal", "rounded"))
}
\arguments{
\item{data}{A numeric vector, matrix, or data frame with observations in rows and variables in columns.}

\item{type}{Character string; either \code{"optimal"} (use the estimated lambda) or \code{"rounded"} (use the rounded lambda).
Default is \code{"optimal"}.}
}
\value{
A list with two components:
\code{data}, a data frame (or vector) of transformed variables;
and \code{lambda}, a numeric vector of the lambda values applied to each variable.
}
\description{
Applies a Box-Cox power transformation to each numeric variable in the input,
estimating or rounding the transformation parameter (lambda) using \code{car::powerTransform}.
}
\examples{
\dontrun{
data <- iris[1:4]
# Apply Box-Cox to the first 4 numeric columns
res <- box_cox_transform(data, type = "rounded")
head(res$data)
res$lambda
}

}
