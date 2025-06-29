% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hash_col.R
\name{hash_col}
\alias{hash_col}
\title{Replaces a column by its hash-value to increase privacy}
\usage{
hash_col(data, column = "caller")
}
\arguments{
\item{column}{column that is supposed to be replaced, defaults to caller}

\item{d}{dataframe with data}
}
\value{
Dataframe with the specified column replaced
}
\description{
Replaces a column by its hash-value to increase privacy
}
