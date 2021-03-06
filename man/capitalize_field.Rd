% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capitalize.r
\name{capitalize_field}
\alias{capitalize_field}
\title{Capitalizing the first letter of a selected field entry}
\usage{
capitalize_field(vec, field, exceptions = NULL)
}
\arguments{
\item{vec}{A Bibtex object}

\item{field}{A selected field (e.g., "title")}

\item{exceptions}{A character vector that contains the words to avoid capitalization.}
}
\value{
A Bibtex object where the selected filed is modified.
}
\description{
Capitalizing the first letter of a selected field entry
}
